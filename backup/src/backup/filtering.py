"""Restic has the following behavior:

1. A directory passed in the include file list will be included,
   along with all its children recursively.
2. A file passed in without its parent directories listed will be
   backed up along with its parent directories, but that doesn't imply
   that any other files in those directories will be backed up.
3. All files passed to --files-from are included in each snapshot JSON
   so minimizing that makes some commands go faster and reduces storage used on
   each backup. Testing shows about 11 MB with about 100k files when including all
   paths, 3.5 MB when de-duplicating parent directories.
"""
import logging
import os
from pathlib import Path
from typing import List


logger = logging.getLogger(__name__)


EXCLUDE_IF_PRESENT_FILE = ".nobackup"


ALWAYS_EXCLUDED_DIRS = [
    "node_modules",
    "__pycache__",
    ".venv",
    ".nox",
]


def get_files(base_path: Path) -> List[str]:
    def onerror(e):
        logger.warning("Error opening %s", e)

    files = []
    #files.extend(str(p) for p in base_path.parents)
    for dirpath, dirnames, filenames in os.walk(str(base_path), onerror=onerror):
        if EXCLUDE_IF_PRESENT_FILE in filenames:
            dirnames.clear()
            continue

        if ".git" in dirnames:
            dirnames[:] = [".git"]
            # Exclude the rest of the worktree, since we backup git repositories
            # with git-bark.
            # TODO: recurse into submodule directories?
            continue

        if ".git" in filenames:
            files.append(f"{dirpath}/.git")
            dirnames.clear()
            continue

        dirnames[:] = [d for d in dirnames if d not in ALWAYS_EXCLUDED_DIRS]

        for f in filenames:
            path = f"{dirpath}/{f}"
            files.append(path)

    return files


def get_minimal_files(base_path: Path) -> List[str]:
    """Get the minimal set of files that does not include excluded files.

    Whether an empty directory will be included in the backup set depends on whether
    it contains any files and can be collapsed into a higher-level tree.
    """
    def onerror(e):
        logger.warning("Error opening %s", e)

    excluded_files = []
    files = []
    for dirpath, dirnames, filenames in os.walk(str(base_path), onerror=onerror):
        if EXCLUDE_IF_PRESENT_FILE in filenames:
            dirnames.clear()
            excluded_files.append(dirpath)
            continue

        if ".git" in dirnames:
            # Exclude the rest of the worktree, since we backup git repositories
            # with git-bark.
            for d in dirnames:
                if d == ".git":
                    continue
                excluded_files.append(f"{dirpath}/{d}")
            for f in filenames:
                excluded_files.append(f"{dirpath}/{f}")

            dirnames[:] = [".git"]
            # TODO: recurse into submodule directories?
            continue

        if ".git" in filenames:
            for d in dirnames:
                excluded_files.append(f"{dirpath}/{d}")
            for f in filenames:
                if f == ".git":
                    continue
                excluded_files.append(f"{dirpath}/{f}")

            files.append(f"{dirpath}/.git")
            dirnames.clear()
            continue

        new_dirnames = []
        for d in dirnames:
            if d in ALWAYS_EXCLUDED_DIRS:
                excluded_files.append(f"{dirpath}/{d}")
                continue
            new_dirnames.append(d)
        dirnames[:] = new_dirnames

        for f in filenames:
            path = f"{dirpath}/{f}"
            files.append(path)

    # At this point I have a list of files to backup and a list of files to exclude.
    # Generate a lookup table for the files to exclude.
    excluded_paths = set()
    for p in excluded_files:
        excluded_paths.add(p)

        p_path = Path(p)
        for parent in p_path.parents:
            excluded_paths.add(str(parent))

    new_files = set()
    for p in files:
        path = Path(p)
        for parent in path.parents:
            if str(parent) not in excluded_paths:
                new_files.add(str(parent))
                break
        else:
            new_files.add(p)

    return sorted(new_files)
