import json
import os
import re
import subprocess
from pathlib import Path
from typing import List

import pytest

from backup.restic import Restic
from .util import jsonl_loads, make_tree


@pytest.fixture
def restic_repo_path(tmp_path_factory) -> Path:
    return tmp_path_factory.mktemp("repo")


@pytest.fixture
def restic_repo(tmp_path_factory, restic_repo_path) -> Restic:
    cache_path = tmp_path_factory.mktemp("repo_cache")
    env = {
        "RESTIC_PASSWORD": "Abcd1234",
        "RESTIC_REPOSITORY": str(restic_repo_path),
        "RESTIC_CACHE_DIR": str(cache_path),
    }
    restic = Restic(env)
    restic.run(["init"])
    return restic


def test_escaping(tmp_path_factory, restic_repo):
    paths = [
        ("env$env/1", True),  # Could match "envtest/1" if $env is expanded to "test"
        ("envtest/1", False),  # Could be matched by "env$env/1" if $env is "test"
        ("backup-dir/*", True),  # Could match files
        ("backup-dir/1", False),  # Could be matched by *
        ("backup-dir/**", True),  # Could match files in folders
        ("backup-dir/subdir/1", False),  # Could be matched by **
        ("**/splat_prefixed", True),  # Could match any "splat_prefixed" in any folder
        ("backup-dir-2/splat_prefixed", False),  # Could be matched by "**/splat_prefixed"
        (" a ", True),  # Could match "a" or " a" if trimmed
        ("a", False),  # Could be matched by " a " if trimmed
        (" a", False),  # Could be matched by " a " if trimmed
        ("?a", True),  # Could match " a" or "ba" if interpreted
        ("ba", False),  # Could be matched by "?a" if interpreted
        ("a\nb", True),  # Could match "a" and "b" if written as-is
        ("ab\n", True),  # Could match "ab" if written as-is
        ("ab", False),  # Could be matched by "ab\n" if written as-is or regex is wrong
        ("b", False),  # Could be matched by "a\nb" if written as-is
        ("[c]", True),  # Could match "c", as a character class
        ("c", False),  # Could be matched by "[c]" if not escaped
    ]
    if os.name != "nt":
        paths.extend([
            ("\\[c]", True),  # Could match "\c", if not escaped correctly
            ("\\c", False),  # Could be matched by "\\[c]", if not escaped correctly
        ])

    backup_src_path = tmp_path_factory.mktemp("backup_src")
    backup_paths = []
    for path, backup in paths:
        p = backup_src_path / path
        p.parent.mkdir(exist_ok=True, parents=True)
        p.touch()

        if backup:
            backup_paths.append(str(p))

    restic_repo.backup(backup_paths, env={"env": "test"})

    result = restic_repo.run(["--json", "ls", "latest"], stdout=subprocess.PIPE)
    files = jsonl_loads(result.stdout)
    stored_paths = {o["path"] for o in files if o["struct_type"] == "node"}

    for path, backup in paths:
        p = backup_src_path / path
        if backup:
            assert str(p) in stored_paths, stored_paths
        else:
            assert str(p) not in stored_paths, stored_paths


def test_exclude_escaping():
    pass


def test_backup_also_includes_directories(
    ok, tmp_path_factory, restic_repo, restic_repo_path
):
    """Restic treats incoming directory paths as a call to recursively store
    everything in that directory.

    This test checks that behavior.
    """
    tmp_path = tmp_path_factory.mktemp("src")
    files = [
        "a/b/c" > ok,
        "a/c/.nobackup" > ok,
    ]
    make_tree(files, tmp_path)
    # Input files only include a/b/c and parents.
    restic_input = ["a/b/c", "a", "a/b"]
    restic_input = [str(tmp_path / p) for p in restic_input]

    restic_repo.backup(restic_input)

    result = restic_repo.run(["--json", "ls", "latest"], stdout=subprocess.PIPE)
    files = jsonl_loads(result.stdout)
    stored_paths = {o["path"] for o in files if o["struct_type"] == "node"}

    def get_expected(paths):
        expected = {str(tmp_path)}
        # Restic doesn't include the root directory in its output, but
        # does include every parent directory.
        expected.update(str(p) for p in tmp_path.parents if str(p) != "/")
        for p in paths:
            new_path = tmp_path / p
            expected.add(str(new_path))
            for parent in new_path.parents:
                if parent == tmp_path:
                    break
                expected.add(str(parent))
        return expected

    assert get_expected(list(ok)) == stored_paths


def test_backup_uses_latest_parent(ok, restic_repo, tmp_path_factory):
    tmp_path = tmp_path_factory.mktemp("src")
    files = [
        "a" > ok,
        "b" > ok,
        "c",
    ]
    make_tree(files, tmp_path)
    # First set of input files only include a subset of files.
    restic_input = list(ok)
    restic_input = [str(tmp_path / p) for p in restic_input]

    restic_repo.backup(restic_input)

    all_restic_input = [str(tmp_path / p) for p in files]
    result = restic_repo.backup(all_restic_input, stdout=subprocess.PIPE)

    output = result.stdout.decode("utf-8")
    m = re.search(r"Files:\s+(\d+) new,\s+(\d+) changed,\s+(\d+) unmodified", output)
    assert m, f"Could not find 'Files' in {output}"
    new, changed, unmodified = map(int, m.groups())
    assert new == 1, output
    assert changed == 0, output
    assert unmodified == 2, output

    m = re.search(r"Dirs:\s+(\d+) new,\s+(\d+) changed,\s+(\d+) unmodified", output)
    assert m, f"Could not find 'Dirs' in {output}"
    new, changed, unmodified = map(int, m.groups())
    assert new == 0, output
    assert changed >= 0, output
    assert unmodified >= 0, output


def test_snapshots_include_full_input_paths():
    pass


def test_restic_backup_considers_changed_files_from_new():
    # For https://github.com/restic/restic/issues/2246
    pass


def test_restic_backup_does_not_recurse_into_excluded_subdirs():
    pass
