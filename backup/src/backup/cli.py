import json
import logging
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Optional

import click
import yaml
from pydantic import BaseModel
from statsd import StatsClient

from .filtering import get_files
from .restic import Restic


logger = logging.getLogger(__name__)
statsd = StatsClient()


class Config(BaseModel):
    # Root directory from which files are selected.
    base_directory: str
    # Output of `env_command`, which is executed to get environment
    # variables used for running restic.
    env: Dict[str, str]
    # Directories to be excluded. Absolute paths are considered relative
    # to the base_directory, relative paths match any subdirectory (recursively).
    exclude_dirs: List[str] = []
    # Options to be provided to restic like `-o k=v`
    options: Dict[str, str] = {}

    class Config:
        extra = "forbid"


def read_config(path: str) -> Config:
    config = Path(path)
    config_text = config.read_text(encoding="utf-8")
    if config.suffix == ".json":
        data = json.loads(config_text)
    elif config.suffix in {".yml", ".yaml"}:
        data = yaml.safe_load(config_text)
    else:
        raise RuntimeError(
            f"Unsupported file type {config.suffix}"
        )

    try:
        env_command = data.pop("env_command")
    except KeyError:
        env = {}
    else:
        result = subprocess.run(
            env_command, check=True, shell=True, stdout=subprocess.PIPE
        )
        env = json.loads(result.stdout.decode("utf-8"))
    return Config(**data, env=env)


config: Optional[Config] = None


@click.group()
@click.option(
    "--config-file", "-f", type=click.Path(exists=True)
)
def main(config_file: str):
    global config
    logging.basicConfig(level=logging.DEBUG)
    config = read_config(config_file)


@main.command("backup")
@click.option("--dry-run/--no-dry-run")
def backup(dry_run):
    restic = Restic(config.env, options=config.options)

    if restic.need_init():
        logger.info("Running `restic init`")
        restic.run(["init"])

    logger.info("Retrieving paths to back up")
    files = get_files(Path(config.base_directory), config.exclude_dirs)

    logger.info("Starting backup")
    with statsd.timer("backup_time"):
        restic.backup(files, snapshot_path=config.base_directory, dry_run=dry_run)

    logger.info("Backup finished")


@main.command("maintain")
def maintain():
    """Remove snapshots according to hard-coded schedule.
    """
    restic = Restic(config.env, options=config.options)

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


@main.command("ls-from-config")
def ls_from_config():
    files = get_files(Path(config.base_directory), config.exclude_dirs)
    print(json.dumps(files, separators=(",", ":")))


@main.command("restic")
@click.argument("args", nargs=-1)
def restic(args):
    """Run restic, populating configuration.
    """
    restic = Restic(config.env)

    try:
        restic.run(args)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)
