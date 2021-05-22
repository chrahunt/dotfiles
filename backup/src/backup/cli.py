import json
import logging
import subprocess
import sys
from pathlib import Path
from typing import Dict, List

import click
from pydantic import BaseModel
from statsd import StatsClient

from .filtering import get_files
from .restic import Restic


logger = logging.getLogger(__name__)
statsd = StatsClient()


class Config(BaseModel):
    # Root directory from which files are selected.
    base_directory: str
    # Command executed in shell to get environment variables when
    # executing restic.
    env_command: str
    # Options to be provided to restic like `-o k=v`
    options: Dict[str, str] = {}

    class Config:
        extra = "forbid"


def read_config() -> Config:
    config = Path.home() / ".backup" / "config.json"
    if not config.exists():
        raise RuntimeError(f"Configuration must exist at {config}!")
    data = json.loads(config.read_text(encoding="utf-8"))
    return Config(**data)


@click.group()
def main():
    logging.basicConfig(level=logging.DEBUG)


ALWAYS_EXCLUDED_DIRS = [
    "node_modules",
    "__pycache__",
    ".venv",
    ".nox",
]


@main.command("backup")
@click.option("--dry-run/--no-dry-run")
def backup(dry_run):
    config = read_config()
    result = subprocess.run(
        config.env_command, check=True, shell=True, stdout=subprocess.PIPE
    )
    env = json.loads(result.stdout.decode("utf-8"))
    restic = Restic(env, options=config.options)

    if restic.need_init():
        logger.info("Running `restic init`")
        restic.run(["init"])

    logger.info("Retrieving paths to back up")
    files = get_files(Path(config.base_directory), ALWAYS_EXCLUDED_DIRS)

    logger.info("Starting backup")
    with statsd.timer("backup_time"):
        restic.backup(files, snapshot_path=config.base_directory, dry_run=dry_run)

    logger.info("Backup finished")


@main.command("maintain")
def maintain():
    """Remove snapshots according to hard-coded schedule.
    """
    config = read_config()
    result = subprocess.run(
        config.env_command, check=True, shell=True, stdout=subprocess.PIPE
    )
    env = json.loads(result.stdout.decode("utf-8"))
    restic = Restic(env, options=config.options)

    if restic.need_init():
        logger.info("Repository has not been initialized")
        return

    restic.run(
        [
            "forget",
            "--keep-within",
            "5d",
            "--keep-daily",
            "14",
            "--keep-weekly",
            "5",
            "--keep-monthly",
            "12",
            "--keep-yearly",
            "2",
        ],
    )


@main.command("ls")
@click.option("--base-directory", type=str, required=True)
@click.option("--exclude-dir", multiple=True, default=[])
def ls(base_directory, exclude_dir: List[str]):
    files = get_files(Path(base_directory), exclude_dir)
    print(json.dumps(files, separators=(",", ":")))


@main.command("restic")
@click.argument("args", nargs=-1)
def restic(args):
    """Run restic, populating configuration.
    """
    config = read_config()
    result = subprocess.run(
        config.env_command, check=True, shell=True, stdout=subprocess.PIPE
    )
    env = json.loads(result.stdout.decode("utf-8"))
    restic = Restic(env)

    try:
        restic.run(args)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)
