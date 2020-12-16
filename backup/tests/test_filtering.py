import hashlib
import os
from pathlib import Path
from typing import Dict

import pytest

from backup.filtering import (
    ALWAYS_EXCLUDED_DIRS,
    EXCLUDE_IF_PRESENT_FILE,
    get_files,
    get_minimal_files,
)
from .util import make_tree


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


def get_expected(paths, base_path):
    return {str(base_path / p) for p in paths}


def test_excludes_empty_directories(ok, tmp_path):
    # We don't really care about empty directories, and it makes our
    # processing easier to exclude them.
    files = [
        "1" > ok,
        "a/1" > ok,
        "b/",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_includes_gitdir_contents(ok, tmp_path):
    files = [
        "1" > ok,
        "a/b" > ok,
        "repo/.git/config" > ok,
        "repo/.git/logs/HEAD" > ok,
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_excludes_git_worktree_contents(ok, tmp_path):
    files = [
        "1" > ok,
        "a/b" > ok,
        "repo/.git/config" > ok,
        "repo/README.md",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_excludes_nobackup_directories(ok, tmp_path):
    files = [
        "a/1" > ok,
        f"b/{EXCLUDE_IF_PRESENT_FILE}",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


@pytest.mark.parametrize("dirname", ALWAYS_EXCLUDED_DIRS)
def test_excludes_excluded_dirs(dirname, ok, tmp_path):
    files = [
        "a/1" > ok,
        "b/1" > ok,
        "b/c/1" > ok,
        f"b/{dirname}/index.js",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_includes_git_file(ok, tmp_path):
    files = [
        "repo/.git" > ok,
        "repo/README.md",
        "repo/src/hello.cpp",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_recurses_into_directories(ok, tmp_path):
    files = [
        "a/b/c" > ok,
        "1/2/3" > ok,
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_get_minimal_files_basics(ok, tmp_path):
    files = [
        "a/1/" > ok,
        "a/1/2",
        "a/1/3",
        # Should block a/
        f"a/2/{EXCLUDE_IF_PRESENT_FILE}",
        "b/1/2" > ok,
        # Should block b/1/
        f"b/1/3/{EXCLUDE_IF_PRESENT_FILE}",
        "c/" > ok,
        "c/1",
        "d/.git" > ok,
        # Should not be chosen since it is a sibling to a .git file.
        "d/1",
        "e/.git/" > ok,
        "e/.git/1",
        # Should not be chosen since it is a sibling to a .git directory.
        "e/1",
    ]
    for i, d in enumerate(ALWAYS_EXCLUDED_DIRS):
        files.extend([
            f"{i}/a/1" > ok,
            # Should block {i}/a/
            f"{i}/a/{d}/1",
            f"{i}/b/1" > ok,
            f"{i}/b/2/" > ok,
            f"{i}/b/2/1",
            # Should block {i}/b/
            f"{i}/b/{d}/1",
        ])
    make_tree(files, tmp_path)

    result = get_minimal_files(tmp_path)
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)
