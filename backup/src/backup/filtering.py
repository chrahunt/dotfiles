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
