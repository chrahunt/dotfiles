import json
import shlex
import subprocess
import sys
import textwrap
from pathlib import Path
from string import Template
from typing import Any, Callable, Dict

import pytest
import yaml
from click.testing import CliRunner

from backup.cli import main
from backup.restic import Restic
from .util import expected_restic_output, jsonl_loads, make_tree


@pytest.fixture
def runner():
    def invoke(args):
        return runner_.invoke(main, args)

    runner_ = CliRunner()
    # Move somewhere else so bugs don't get into files in our cwd.
    with runner_.isolated_filesystem():
        yield invoke


@pytest.mark.parametrize("config,message", [
    ({}, "missing"),
    ({"directory": ""}, "extra fields not permitted"),
])
def test_bad_config_fails(config, message, monkeypatch, tmp_path_factory, runner):
    home = tmp_path_factory.mktemp("home")
    src = tmp_path_factory.mktemp("src")
    # Override config file location.
    monkeypatch.setenv("HOME", str(home))
    config_path = home / ".backup" / "config.json"
    config_path.parent.mkdir()
    config_path.write_text(json.dumps(config), encoding="utf-8")
    result = runner(["-f", str(config_path), "backup"])
    assert result.exit_code != 0
    assert message in str(result.exception)


def test_unknown_config_format_fails(tmp_path_factory, runner):
    home = tmp_path_factory.mktemp("home")
    config_name = "config.txt"
    config_path = make_config(home, config_name, {}, "/")
    result = runner(["-f", str(config_path), "backup"])
    assert result.exit_code != 0
    assert "Unsupported file type" in str(result.exception)


@pytest.mark.parametrize("config_name", ["config.yml", "config.yaml"])
def test_yaml_config_file_works(tmp_path_factory, runner, config_name):
    home = tmp_path_factory.mktemp("home")
    config_path = make_config(home, config_name, {}, "/", converter=yaml.safe_dump)
    result = runner(["-f", str(config_path), "restic"])
    assert result.exit_code == 0


def make_config(
    temp_dir: Path,
    config_name: str,
    env: Dict[str, str],
    base_directory: str,
    converter: Callable[[Any], str] = json.dumps,
):
    helper_script = temp_dir / "helper.py"
    helper_script.write_text(
        textwrap.dedent(
            Template(
                """
                import json


                data = $data
                print(json.dumps(data))
                """
            ).substitute(data=json.dumps(env))
        ),
        encoding="utf-8",
    )
    command = [sys.executable, str(helper_script)]
    command_args = map(shlex.quote, command)
    command_text = " ".join(command_args)

    # Test that we're actually executing env_command to
    # get values, by referring to them in config.
    config_env = {
        k: f"{{{k}}}" for k in env.keys()
    }
    config = {
        "base_directory": base_directory,
        "env_command": command_text,
        "options": {},
        "env": config_env,
    }
    config_path = temp_dir / config_name
    config_path.write_text(converter(config))
    return config_path


@pytest.fixture
def write_config(monkeypatch, tmp_path_factory):
    """For reasons, some of our configuration is retrieved dynamically by invoking
    a script or executable defined in a JSON file.
    """
    home = tmp_path_factory.mktemp("home")
    # Override config file location.
    monkeypatch.setenv("HOME", str(home))
    config_path = home / ".backup" / "config.json"
    config_path.parent.mkdir()

    def _write_config(base_directory: str, env: Dict[str, str]):
        return make_config(home, "config.json", env, base_directory)

    return _write_config


def test_initializes_restic_repo(write_config, tmp_path_factory, runner):
    src = tmp_path_factory.mktemp("src")
    (src / "1").touch()
    (src / "2").touch()
    repo = tmp_path_factory.mktemp("repo")
    env = {
        "RESTIC_PASSWORD": "qwerty1234",
        "RESTIC_REPOSITORY": str(repo),
    }
    config = write_config(str(src), env)
    result = runner(["-f", str(config), "backup"])
    assert result.exit_code == 0

    # Now validate with restic outside the wrapper script.
    restic = Restic(env)
    result = restic.run(["--json", "ls", "latest"], stdout=subprocess.PIPE)
    files = jsonl_loads(result.stdout)
    paths = [f["path"] for f in files if f["struct_type"] == "node"]
    assert str(src / "1") in paths
    assert str(src / "2") in paths


def test_respects_some_filter_rules(ok, write_config, tmp_path_factory, runner):
    """Basic checks that, for example, a .nobackup directory isn't
    backed up.
    """
    src = tmp_path_factory.mktemp("src")

    files = [
        "a/ignored/.nobackup",
        "a/emptydir/",
        "a/file" > ok,
        "a/subdir/file" > ok,
        "repo/ignore-me",
        "repo/.git/back-me-up" > ok,
        "repo2/ignore-me-too",
        "repo2/.git" > ok,
    ]
    expected_files = list(ok)
    make_tree(files, src)

    repo = tmp_path_factory.mktemp("repo")
    env = {
        "RESTIC_PASSWORD": "qwerty1234",
        "RESTIC_REPOSITORY": str(repo),
    }
    config = write_config(str(src), env)
    result = runner(["-f", str(config), "backup"])
    assert result.exit_code == 0

    # Now validate with restic outside the wrapper script.
    restic = Restic(env)
    result = restic.run(["--json", "ls", "latest"], stdout=subprocess.PIPE)
    files = jsonl_loads(result.stdout)
    paths = {f["path"] for f in files if f["struct_type"] == "node"}
    expected = expected_restic_output(expected_files, src)
    assert expected == paths
