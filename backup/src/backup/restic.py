import logging
import os
import shlex
import subprocess
from contextlib import contextmanager
from itertools import chain
from pathlib import Path
from socket import gethostname
from tempfile import TemporaryDirectory
from typing import Iterator, List, Optional


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

    def run(self, args, **kwargs) -> subprocess.CompletedProcess:
        kwargs.setdefault("check", True)

        env = kwargs.pop("env", {})
        kwargs["env"] = {**os.environ, **self._env, **env}

        args = ["restic", *self._get_options(), *args]

        logger.debug("Running: %s", " ".join(shlex.quote(a) for a in args))
        return subprocess.run(args, **kwargs)

    def backup(self, paths: List[str], **kwargs) -> subprocess.CompletedProcess:
        # In order to avoid a re-scan, we need to provide an explicit parent
        # snapshot. In order to filter the parent we need to know the user and
        # host from which we're running (assume 1 backup source for now).
        # https://github.com/restic/restic/issues/2246
        # To avoid differences between the default Go hostname retrieval and Python,
        # we just always provide an explicit hostname.
        args = self._host_args()
        latest_snapshot = self._latest_snapshot()
        if latest_snapshot is not None:
            args.extend(["--parent", latest_snapshot])
        if kwargs.pop("dry_run", False):
            args.append("--dry-run")
            # The --dry-run flag from https://github.com/restic/restic/pull/3300
            # doesn't show the files backed up unless verbose logging is enabled
            # (at level 2).
            args.append("-vv")
            # Testing json log output.
            args.append("--json")
        try:
            snapshot_path = kwargs.pop("snapshot_path")
        except KeyError:
            pass
        else:
            args.extend(["--set-path", snapshot_path])
        with escaped_paths(paths) as path_args:
            args.extend(path_args)
            return self.run(["backup", *args], **kwargs)

    def _host_args(self) -> List[str]:
        return ["--host", gethostname()]

    def _latest_snapshot(self) -> Optional[str]:
        host_args = self._host_args()
        # With our current scheme of passing all files to restic, all paths
        # end up in each snapshot. Using --json may make the most sense from an
        # interoperability point-of-view, but we don't actually want to read and
        # parse hundreds of MB of JSON text to figure out the latest snapshot.

        # --compact strips the paths from the output
        # "latest" will retrieve only the latest item, so we should be left with
        # one data row
        result = self.run(
            ["snapshots", "--compact", *host_args, "latest"], stdout=subprocess.PIPE
        )
        text = result.stdout.decode("utf-8")

        try:
            footer_divider = text.rindex("\n-----")
        except ValueError:
            # If there are no snapshots, the table is not printed.
            return None

        last_row_start = text.rindex("\n", 0, footer_divider) + 1
        snapshot_id_end = text.index(" ", last_row_start)
        return text[last_row_start:snapshot_id_end]

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
