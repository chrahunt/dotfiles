"""Git repository information consists of:

1. Files in the `.git` repository directory
2. Files in the working tree

Backing up a git repository means persisting it in a way so it can
be recovered later. There are several possible approaches, with
different trade-offs:

1. `git bundle create` - the resulting bundle does not include modified files in the
   working tree, `.git/hooks`, `.git/config`, `.git/info/*`, or the reflog.
2. `git clone --mirror` - this does not include repository-local files and is also
   pickier about which objects it copes.
3. backup the working tree and `.git` file/directory - this will likely copy
   build artifacts (there can be a lot). What build artifacts to exclude is going to
   vary by project, and some backup applications (duplicity, rclone, restic) don't seem
   to support directory-specific exclude files (like reading `.gitignore`).
4. backup just the `.git` file/directory - this does not include modified files
   in the working tree.

This script improves the `.git` directory backup approach, saving all modifications to
the working tree to the repository. The regular index is not modified, so in-progress
rebase/merge/staging is not interrupted.

Any ignored files will not be included.
"""
import argparse
import os
import subprocess
import sys
import tempfile
import textwrap
from contextlib import contextmanager
from pathlib import Path
from typing import Iterable, List, NamedTuple, Optional


class OutputPrintingError(subprocess.CalledProcessError):
    def __init__(self, exc: subprocess.CalledProcessError):
        super().__init__(exc.returncode, exc.cmd, exc.output, exc.stderr)

    def __str__(self):
        # Already has command and exit code/signal.
        base_text = super().__str__()

        fields = []

        if self.output is not None:
            printable_output = self.output.decode("utf-8", errors="replace")
            output = textwrap.indent(printable_output, "  ")
            fields.append(f"output:\n{output}")

        if self.stderr is not None:
            printable_stderr = self.stderr.decode("utf-8", errors="replace")
            stderr = textwrap.indent(printable_stderr, "  ")
            fields.append(f"stderr:\n{stderr}")

        if fields:
            field_text = textwrap.indent("\n".join(fields), "  ")
            return f"{base_text}\n{field_text}"

        return base_text


@contextmanager
def traced_error_output():
    try:
        yield
    except subprocess.CalledProcessError as e:
        raise OutputPrintingError(e).with_traceback(e.__traceback__) from None


def git(args, **kwargs) -> subprocess.CompletedProcess:
    args.insert(0, "git")
    with traced_error_output():
        return subprocess.run(
            args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True, **kwargs
        )


def git_ref_exists(name: str) -> bool:
    try:
        git(["show-ref", "--quiet", "--verify", name])
    except subprocess.CalledProcessError as e:
        if e.returncode == 1:
            return False
        raise
    else:
        return True


# Any refs under refs/worktree are worktree-specific, and behavior is exactly like we'd
# expect:
# - per-worktree refs (worktrees/<name>/refs/... or refs/...)
# - per-worktree reflogs (worktrees/<name>/logs/... or logs/...)
BACKUP_REF = "refs/worktree/backup"


def backup() -> None:
    """Given a (possibly dirty) worktree, back it up to the repository.
    """
    # Create a new tree object.
    with tempfile.TemporaryDirectory() as d:
        index_file = Path(d) / "INDEX"
        env = os.environ.copy()
        env.update({"GIT_INDEX_FILE": str(index_file)})
        subprocess.run(["git", "add", "--all"], env=env, check=True)
        result = git(["write-tree"], env=env)
    tree_id = result.stdout.decode("utf-8").strip()

    result = git(["cat-file", "tree", tree_id])
    if not result.stdout.strip():
        # We cannot check out an empty tree, so do nothing.
        return

    if git_ref_exists(BACKUP_REF):
        result = git(["show", "--quiet", "--pretty=format:%T", BACKUP_REF])
        current_tree_id = result.stdout.decode("utf-8").strip()
        if current_tree_id == tree_id:
            # If the backup is already current, don't make a new commit.
            return

    message = b"backup"
    result = git(["commit-tree", "--no-gpg-sign", tree_id], input=message)
    commit_id = result.stdout.decode("utf-8").strip()

    git(["update-ref", "-m", "new backup", "--create-reflog", BACKUP_REF, commit_id])


def _parse_ls_files_z(output: bytes) -> Iterable[str]:
    output = output.rstrip(b"\0")
    if not output:
        return
    records = output.split(b"\0")
    for record in records:
        _perms, _hash, _stage, filename = record.decode("utf-8").split(maxsplit=3)
        yield filename


class StatusEntry(NamedTuple):
    state_1: str
    state_2: str
    path: str
    orig_path: Optional[str]


MULTI_FIELD_STATES = {"C", "R"}


def _parse_status_z(output: bytes) -> Iterable[StatusEntry]:
    output = output.rstrip(b"\0")
    # No change in index or working tree.
    if not output:
        return
    fields = iter(output.split(b"\0"))
    # field is the first part of an entry, i.e. XY PATH
    for field in fields:
        decoded_field = field.decode("utf-8")
        state_1, state_2, path = decoded_field[0], decoded_field[1], decoded_field[3:]
        orig_path = None
        if state_1 in MULTI_FIELD_STATES or state_2 in MULTI_FIELD_STATES:
            orig_path = next(fields).decode("utf-8")
        yield StatusEntry(state_1, state_2, path, orig_path)


def restore() -> None:
    """Given a worktree that has been backed up, restore it.

    "Backed up" means:
    1. `backup` was executed on the worktree
    2. git knows how to locate the repository
    3. the worktree
    3. the current state of the provided `path` is that the copied `.git`
       directory/file is present and any other files present in the worktree can be
       disregarded in favor of the backed up working tree state.
    """
    # Our goal is to support a worktree with no (consequential) contents. This means
    # no added/updated/renamed/untracked files.
    # git status alone doesn't let us differentiate between unmerged paths and paths that
    # are different from the index, so we retrieve the unmerged paths separately.
    result = git(["rev-parse", "--show-toplevel"])
    worktree_dir = Path(result.stdout.decode("utf-8").rstrip("\n"))

    # -z: NUL-terminated
    result = git(["ls-files", "-z", "--unmerged"])
    unmerged_paths = set(_parse_ls_files_z(result.stdout))
    for path in unmerged_paths:
        if (worktree_dir / path).exists():
            raise RuntimeError("Cannot restore worktree with unresolved paths present")

    # --no-optional-locks: Do not touch the index.
    # -z: NUL-separated fields/NUL-terminated records.
    result = git(["--no-optional-locks", "status", "-z"])
    for entry in _parse_status_z(result.stdout):
        if entry.path in unmerged_paths:
            continue
        # Relative to the index, all files must be considered deleted.
        if entry.state_2 != "D":
            raise RuntimeError("Working tree must not have any files!")

    if not git_ref_exists(BACKUP_REF):
        # No ref, no action.
        return

    with tempfile.TemporaryDirectory() as d:
        index_file = Path(d) / "INDEX"
        env = os.environ.copy()
        # Temporarily set git index to a different location so we don't clobber the existing one.
        env.update({"GIT_INDEX_FILE": str(index_file)})
        git(["checkout", BACKUP_REF, "."], env=env)


def main(argv: List[str] = sys.argv[1:]):
    parser = argparse.ArgumentParser(
        description="Backup in-progress git repository files"
    )
    subparsers = parser.add_subparsers(
        description="command", dest="command", required=True
    )

    backup_parser = subparsers.add_parser("backup")
    backup_parser.set_defaults(func=backup)

    restore_parser = subparsers.add_parser("restore")
    restore_parser.set_defaults(func=restore)

    args = parser.parse_args(argv)
    args.func()


if __name__ == "__main__":
    main()
