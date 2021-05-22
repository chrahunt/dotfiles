import hashlib
import os
from pathlib import Path
from typing import Dict

from backup.filtering import (
    EXCLUDE_IF_PRESENT_FILE,
    get_files,
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

    result = get_files(tmp_path, [])
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

    result = get_files(tmp_path, [])
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

    result = get_files(tmp_path, [])
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_excludes_nobackup_directories(ok, tmp_path):
    files = [
        "a/1" > ok,
        f"b/{EXCLUDE_IF_PRESENT_FILE}",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path, [])
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_excludes_general_excluded_dirs(ok, tmp_path):
    dirname = "example"
    files = [
        "a/1" > ok,
        "b/1" > ok,
        "b/c/1" > ok,
        f"b/{dirname}/index.js",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path, [dirname])
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_excludes_absolute_excluded_dirs(ok, tmp_path):
    files = [
        "a/1" > ok,
        "b/1",
        "c/b/1" > ok,
        "d/a/1",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path, ["/b", "/d/a"])
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_includes_git_file(ok, tmp_path):
    files = [
        "repo/.git" > ok,
        "repo/README.md",
        "repo/src/hello.cpp",
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path, [])
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)


def test_recurses_into_directories(ok, tmp_path):
    files = [
        "a/b/c" > ok,
        "1/2/3" > ok,
    ]
    make_tree(files, tmp_path)

    result = get_files(tmp_path, [])
    expected = get_expected(list(ok), tmp_path)

    assert set(result) == set(expected)
