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
from collections import defaultdict
from pathlib import Path
from typing import Dict, FrozenSet, Iterable, List

logger = logging.getLogger(__name__)


EXCLUDE_IF_PRESENT_FILE = ".nobackup"


class Exclusions:
    def __init__(
        self, always_excluded_dirs: List[str], dir_excludes_by_dir: Dict[str, List[str]]
    ):
        self._always_excluded_dirs = frozenset(always_excluded_dirs)
        self._dir_excludes_by_dir = dir_excludes_by_dir

    @staticmethod
    def from_exclude_dirs(base_path: str, exclude_dirs: Iterable[str]) -> "Exclusions":
        always_excluded_dirs = []
        exclude_config = defaultdict(list)
        path_prefix = "" if base_path == "/" else base_path
        for d in exclude_dirs:
            # Considered relative to base_path.
            if d.startswith("/"):
                full_path = f"{path_prefix}{d}"
                key, dirname = full_path.rsplit("/", maxsplit=1)
                exclude_config[key].append(dirname)
            # Otherwise, a single path part considered relative to any directory.
            else:
                if "/" in d:
                    raise ValueError(
                        f"{d!r} must not contain '/' unless it is absolute."
                    )
                always_excluded_dirs.append(d)
        return Exclusions(always_excluded_dirs, exclude_config)

    def get_exclude_dirs(self, path: str) -> FrozenSet[str]:
        # This is the less common path, so use the more efficient check compared to
        # try/except.
        if path in self._dir_excludes_by_dir:
            return self._always_excluded_dirs.union(self._dir_excludes_by_dir[path])
        return self._always_excluded_dirs


def get_files(base_path: Path, exclude_dirs: Iterable[str] = ()) -> List[str]:
    """
    :param exclude_dirs: list of paths, of directories to exclude
    """
    exclusions = Exclusions.from_exclude_dirs(str(base_path), exclude_dirs)
    return _get_files(base_path, exclusions)


def _get_files(base_path: Path, exclusions: Exclusions) -> List[str]:
    def onerror(e):
        logger.warning("Error opening %s", e)

    files = []
    for dirpath, dirnames, filenames in os.walk(str(base_path), onerror=onerror):
        if EXCLUDE_IF_PRESENT_FILE in filenames:
            dirnames.clear()
            continue

        # git repositories get special treatment. The git repository itself can have
        # a lot of important data that is only available locally (commits, hooks,
        # scripts) in .git/. The work tree usually has build artifacts, though, which
        # we don't want to back up. To satisfy these two cases, we back up untracked and
        # modified files to the git repo with git-bark and then back up the .git
        # directory. Any local ignored setup needed for development should be documented
        # or scripted.
        if ".git" in dirnames:
            dirnames[:] = [".git"]
            # TODO: recurse into submodule directories?
            continue

        # Keep track of the location of separate worktrees or symlinked repositories.
        if ".git" in filenames:
            files.append(f"{dirpath}/.git")
            dirnames.clear()
            continue

        excluded_dirs = exclusions.get_exclude_dirs(dirpath)
        dirnames[:] = [d for d in dirnames if d not in excluded_dirs]

        path_prefix = "" if dirpath == "/" else dirpath
        for f in filenames:
            path = f"{path_prefix}/{f}"
            files.append(path)

    return files
