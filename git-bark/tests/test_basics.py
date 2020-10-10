import difflib
import hashlib
import logging
import os
import shutil
import subprocess
from contextlib import ExitStack, contextmanager
from itertools import chain, repeat
from pathlib import Path
from typing import Dict

import git_bark.__main__ as git_bark
import pytest

from .lib.subprocess_script import Script

logger = logging.getLogger(__name__)


class Git:
    def __init__(self, worktree: Path):
        self.worktree = worktree

    def run(self, args, **kwargs):
        kwargs.setdefault("cwd", self.worktree)
        kwargs.setdefault("check", True)
        args.insert(0, "git")
        return subprocess.run(args, **kwargs)

    def status(self) -> str:
        # --no-optional-locks prevents `git status` from updating the index, which
        # would mess up our file integrity verification.
        return self.run(["--no-optional-locks", "status"], capture_output=True).stdout


@pytest.fixture
def ctx():
    with ExitStack() as ctx:
        yield ctx


def snapshot(d: Path) -> Dict[str, str]:
    result: Dict[str, str] = {}
    for dirname, dirnames, filenames in os.walk(d):
        result[dirname] = ""
        for filename in filenames:
            path = os.path.join(dirname, filename)
            with open(path, "rb") as f:
                hash = hashlib.sha1(f.read()).hexdigest()
            result[str(path)] = hash
    return result


def diff_snapshots(a, b):
    # Helper for visualizing the actual difference.
    def make_lines(snapshot):
        names = sorted(snapshot)
        return [f"{name}: {snapshot[name]}" for name in names]

    a_lines = make_lines(a)
    b_lines = make_lines(b)
    return difflib.unified_diff(a_lines, b_lines, n=0, lineterm="")


def remove_git_files(worktree: Path, files: Dict[str, str]) -> Dict[str, str]:
    return {k: v for k, v in files.items() if not k.startswith(str(worktree / ".git"))}


@contextmanager
def chdir(path: Path):
    cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(cwd)


def backup_and_verify(git: Git):
    git_status_before = git.status()
    files_before = remove_git_files(git.worktree, snapshot(git.worktree))

    with chdir(git.worktree):
        git_bark.main(["backup"])

    git_status_after = git.status()
    assert git_status_before == git_status_after
    files_after = remove_git_files(git.worktree, snapshot(git.worktree))
    assert files_before == files_after


def restore_and_verify(git: Git):
    """Moves the git repository and restores it in-place, so additional
    assertions can be made.
    """
    git_status_before = git.status()
    files_before = snapshot(git.worktree)

    worktree_backup = git.worktree.with_suffix(".bak")
    shutil.move(git.worktree, worktree_backup)
    src_git_path = worktree_backup / ".git"
    dest_git_path = git.worktree / ".git"
    dest_git_path.parent.mkdir(parents=True)
    # Copy only the git directory, to represent a restored backup.
    if src_git_path.is_dir():
        shutil.copytree(src_git_path, dest_git_path)
    else:
        shutil.copy2(src_git_path, dest_git_path)

    with chdir(git.worktree):
        git_bark.main(["restore"])

    git_status_after = git.status()
    assert git_status_before == git_status_after
    files_after = snapshot(git.worktree)
    assert files_before == files_after


def backup_and_restore(git: Git):
    backup_and_verify(git)
    restore_and_verify(git)


def test_git_no_commits_no_changes(tmp_path):
    git = Git(tmp_path)
    git.run(["init", "."])

    backup_and_restore(git)


def test_git_no_commits_worktree_changes(tmp_path):
    git = Git(tmp_path)
    git.run(["init", "."])

    (tmp_path / "unstaged-file").touch()

    backup_and_restore(git)


def test_git_no_commits_staged_changes(tmp_path):
    git = Git(tmp_path)
    git.run(["init", "."])

    (tmp_path / "staged-file").touch()

    backup_and_restore(git)


def test_git_no_commits_staged_and_worktree_changes(tmp_path):
    git = Git(tmp_path)
    git.run(["init", "."])

    staged_file = tmp_path / "staged-file"
    staged_file.touch()
    staged_file_2 = tmp_path / "staged-file-2"
    staged_file_2.touch()

    git.run(["add", "."])

    staged_file.write_text("1", encoding="utf-8")
    staged_file_2.unlink()
    (tmp_path / "unstaged-file").touch()

    backup_and_restore(git)


def test_git_some_commits_worktree_changes(tmp_path):
    git = Git(tmp_path)
    git.run(["init", "."])

    committed_file = tmp_path / "committed-file"
    committed_file.touch()
    committed_file_2 = tmp_path / "committed-file-2"
    committed_file_2.touch()

    git.run(["add", "."])
    git.run(["commit", "-m", "init"])

    committed_file.write_text("1", encoding="utf-8")
    committed_file_2.unlink()
    (tmp_path / "unstaged-file").touch()

    backup_and_restore(git)


def test_git_some_commits_staged_changes(tmp_path):
    git = Git(tmp_path)
    git.run(["init", "."])

    (tmp_path / "committed-file").touch()

    git.run(["add", "."])
    git.run(["commit", "-m", "init"])

    (tmp_path / "staged-file").touch()

    git.run(["add", "."])

    backup_and_restore(git)


def handle_rebase_break_every_commit(context):
    rebase_file = Path(context.argv[1])
    rebase_text = rebase_file.read_text(encoding="utf-8")
    rebase_commands = (
        l
        for l in (l.strip() for l in rebase_text.splitlines())
        if l and not l.startswith("#")
    )
    new_rebase_commands = chain.from_iterable(zip(rebase_commands, repeat("b")))
    new_rebase_text = "\n".join(new_rebase_commands) + "\n"
    rebase_file.write_text(new_rebase_text, encoding="utf-8")


def test_git_interactive_rebase(tmp_path_factory, ctx):
    tmp_path = tmp_path_factory.mktemp("repo")
    git = Git(tmp_path)
    git.run(["init", "."])

    (tmp_path / "committed-file-1").touch()

    git.run(["add", "."])
    git.run(["commit", "-m", "init"])

    committed_file_2 = tmp_path / "committed-file-2"
    committed_file_2.touch()

    git.run(["add", "."])
    git.run(["commit", "-m", "update"])

    script = ctx.enter_context(Script(handle_rebase_break_every_commit))

    env = os.environ.copy()
    env.update(script.environ)
    env["GIT_SEQUENCE_EDITOR"] = script.command
    git.run(["rebase", "-i", "HEAD^"], env=env)

    committed_file_2.write_text("1", encoding="utf-8")

    backup_and_restore(git)


def test_git_interactive_rebase_conflicted(tmp_path_factory, ctx):
    tmp_path = tmp_path_factory.mktemp("repo")
    git = Git(tmp_path)

    git.run(["init", "."])
    git.run(["commit", "-m", "init", "--allow-empty"])

    committed_file = tmp_path / "committed-file"
    committed_file.write_text("1", encoding="utf-8")

    git.run(["add", "."])
    git.run(["commit", "-m", "update 1"])

    committed_file.write_text("2", encoding="utf-8")

    git.run(["add", "."])
    git.run(["commit", "-m", "update 2"])

    script = ctx.enter_context(Script(handle_rebase_break_every_commit))

    env = os.environ.copy()
    env.update(script.environ)
    env["GIT_SEQUENCE_EDITOR"] = script.command
    git.run(["rebase", "-i", "HEAD^^"], env=env)
    committed_file.write_text("3", encoding="utf-8")
    git.run(["add", "."])
    git.run(["commit", "--amend", "--no-edit"])
    result = git.run(["rebase", "--continue"], check=False)
    # Rebase should have failed, and now we're in a conflicted state.
    assert result.returncode != 0

    backup_and_restore(git)


def test_git_worktree_outstanding_work(tmp_path_factory):
    base_repo = tmp_path_factory.mktemp("base_repo")
    base_git = Git(base_repo)
    base_git.run(["init", "."])

    committed_file = base_repo / "committed-file"
    committed_file.touch()

    base_git.run(["add", "."])
    base_git.run(["commit", "-m", "init"])
    (base_repo / "staged-file").touch()
    base_git.run(["add", "."])
    committed_file.write_text("1", encoding="utf-8")

    worktree = tmp_path_factory.mktemp("worktree")
    base_git.run(["worktree", "add", str(worktree)])
    worktree_git = Git(worktree)
    (worktree / "staged-file-2").touch()
    worktree_git.run(["add", "."])
    committed_file.write_text("2", encoding="utf-8")

    backup_and_restore(worktree_git)


def test_backup_doesnt_backup_again(monkeypatch, tmp_path):
    """Given a backup has already occurred, backing up again shouldn't change
    anything.
    """
    git = Git(tmp_path)
    git.run(["init", "."])

    (tmp_path / "pending-file").touch()

    # Initial backup should create a commit.
    backup_and_verify(git)

    after_backup_1 = snapshot(git.worktree)

    # Commits in the same second will have the same hash, so
    # explicitly set the commit date/time to something different.
    monkeypatch.setenv(
        "GIT_AUTHOR_DATE", "Mon Sep 7 22:22:20 2020 -0400"
    )

    backup_and_verify(git)

    # Subsequent backup should not create a commit.
    after_backup_2 = snapshot(git.worktree)

    assert after_backup_1 == after_backup_2


def test_git_dir_from_envvar():
    pass


def test_restore_fails_with_unmerged_paths():
    pass


def test_restore_fails_with_untracked_files():
    pass


def test_restore_fails_with_undeleted_files():
    pass
