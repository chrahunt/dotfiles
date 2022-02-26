import logging
import os
import shlex
import subprocess
from contextlib import contextmanager
from itertools import chain
from pathlib import Path
from socket import gethostname
from tempfile import TemporaryDirectory
from typing import Iterator, List


logger = logging.getLogger(__name__)


@contextmanager
def escaped_paths(paths: List[str]) -> Iterator[List[str]]:
    with TemporaryDirectory() as d:
        # Restic requires each line have a null-terminator
        from_paths_text = "\x00".join(chain(paths, [""]))
        from_paths_path = Path(d) / "paths.txt"
        from_paths_path.write_text(from_paths_text, encoding="utf-8")
        yield ["--files-from-raw", str(from_paths_path)]


class Restic:
    def __init__(self, env, options=None):
        self._env = env
        self._options = options or {}

    def run(self, args: List[str], **kwargs) -> subprocess.CompletedProcess:
        """
        Run restic, adding configured environment variables and options.

        :param args: passed to subprocess.run (after options are populated)
        :param kwargs: passed to subprocess.run (after `env` is enriched)
        :return: completed restic process
        """
        kwargs.setdefault("check", True)

        env = kwargs.pop("env", {})
        kwargs["env"] = {**os.environ, **self._env, **env}

        args = ["restic", *self._get_options(), *args]

        logger.debug("Running: %s", " ".join(shlex.quote(a) for a in args))
        return subprocess.run(args, **kwargs)

    def backup(
        self,
        paths: List[str],
        dry_run=False,
        snapshot_path: str = None,
        host: str = None,
        **kwargs
    ) -> subprocess.CompletedProcess:
        """
        :param paths: paths of files to back up
        :param dry_run: if true, then no backup is actually done, and files
            are printed
        :param snapshot_path: path to use for snapshot identification (by default,
            restic uses a concatenation of all `paths`)
        :param host: hostname to use in restic command, or `socket.gethostname()`
            by default
        :param kwargs: passed to subprocess.run
        :return: completed restic backup process
        """
        # In order to avoid a re-scan, we need to provide an explicit parent
        # snapshot. In order to filter the parent we need to know the user and
        # host from which we're running (assume 1 backup source for now).
        # https://github.com/restic/restic/issues/2246
        # To avoid differences between the default Go hostname retrieval and Python,
        # we just always provide an explicit hostname.
        if host is None:
            host = gethostname()

        args = ["--host", host]

        if dry_run:
            args.append("--dry-run")
            # The --dry-run flag from https://github.com/restic/restic/pull/3300
            # doesn't show the files backed up unless verbose logging is enabled
            # (at level 2).
            args.append("-vv")
            # Testing json log output.
            args.append("--json")

        if snapshot_path is not None:
            args.extend(["--set-path", snapshot_path])

        with escaped_paths(paths) as path_args:
            args.extend(path_args)
            return self.run(["backup", *args], **kwargs)

    def need_init(self) -> bool:
        # https://github.com/restic/restic/issues/1690
        try:
            self.run(["cat", "config"])
        except subprocess.CalledProcessError:
            return True
        else:
            return False

    def _get_options(self) -> List[str]:
        options = []
        for k, v in self._options.items():
            options.append("-o")
            options.append(f"{k}={v}")
        return options
