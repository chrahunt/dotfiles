import os
import subprocess
import sys
from dataclasses import dataclass
from io import StringIO
from itertools import chain
from pathlib import Path
from typing import Iterable, Iterator, List, Optional, Tuple

import click

DOTFILES_DIR = os.path.expanduser("~/.dotfiles.d")
HOME = os.path.expanduser("~")


@dataclass
class Dotfiles:
    path: str
    package_names: List[str]


def all_dotfiles():
    for dotfile_dir in non_hidden_subdirs(DOTFILES_DIR):
        subdirs = non_hidden_subdirs(dotfile_dir.path)
        # Excludes top-level __pycache__ generated by calls to nox.
        subdirs = [d for d in subdirs if d.name != "__pycache__"]
        subdirs = sorted(d.name for d in subdirs)
        yield Dotfiles(dotfile_dir.path, subdirs)


def non_hidden_subdirs(path):
    with os.scandir(path) as it:
        for entry in it:
            if entry.name.startswith("."):
                continue
            if not entry.is_dir():
                continue
            yield entry


def stow(*args):
    subprocess.run(["stow", *args], check=True)


def build_packages():
    """Some packages have a build step to generate files, this takes
    care of that.
    """
    def package_dirs():
        for dotfiles in all_dotfiles():
            for name in dotfiles.package_names:
                yield Path(dotfiles.path) / name

    def nox_has_session(noxfile: Path, session_name: str) -> bool:
        result = subprocess.run(
            [sys.executable, "-m", "nox", "--list", "-f", str(noxfile)],
            capture_output=True,
        )

        lines = result.stdout.decode("utf-8").splitlines()
        for line in lines:
            if line.startswith(f"* {session_name}"):
                return True
        return False

    def has_applicable_noxfile(package_dir) -> Optional[Path]:
        noxfile = package_dir / "noxfile.py"
        if not noxfile.exists():
            return None
        if not nox_has_session(noxfile, "build"):
            return None
        return noxfile


    for package_dir in package_dirs():
        noxfile = has_applicable_noxfile(package_dir)
        if noxfile:
            print(f"Running build for {package_dir}")
            subprocess.run(
                [sys.executable, "-m", "nox", "-s", "build", "-f", str(noxfile)],
                check=True,
            )


def generate_stow_local_ignore():
    def package_dirs():
        for dotfiles in all_dotfiles():
            for name in dotfiles.package_names:
                yield Path(dotfiles.path) / name

    global_text = get_combined_text(".stow-local-ignore.global")
    for package_dir in package_dirs():
        buf = StringIO("# global\n")
        buf.write(global_text)

        local_ignore = package_dir / ".stow-local-ignore.local"
        if local_ignore.exists():
            buf.write("\n\n")
            buf.write("# local\n")
            buf.write(local_ignore.read_text("utf-8"))

        output_local_ignore = package_dir / ".stow-local-ignore"
        output_local_ignore.write_text(buf.getvalue(), encoding="utf-8")


def combine_files():
    """Some utilities don't have a mechanism for spreading configuration
    across multiple files without specifying all of them. For these, we
    accumulate contents from different app-specific directories into a
    single file.
    """
    configs = [
        (".editrc.d", "editline/.editrc"),
        (".gitignore_global.d", "git/.gitignore_global"),
    ]

    for dirname, output_name in configs:
        text = get_combined_text(f"{dirname}/*")
        output_path = Path(DOTFILES_DIR) / "main" / output_name
        output_path.write_text(text, encoding="utf-8")


def get_combined_text(pattern):
    def match_files(pattern):
        pattern = f"*/{pattern}"
        for dotfiles in all_dotfiles():
            yield from Path(dotfiles.path).glob(pattern)

    def combine_text(files):
        files = ordered_by_name(files)
        entries = with_contents(files)
        text_chunks = map(format_entry, entries)
        return "\n".join(chain.from_iterable(text_chunks))

    files = match_files(pattern)
    return combine_text(files)


def ordered_by_name(paths):
    # type: (Iterable[str]) -> List[str]
    return sorted(paths, key=lambda p: list(reversed(os.path.split(p))))


def with_contents(paths):
    # type: (Iterable[str]) -> Iterator[Tuple[str, str]]
    for path in paths:
        with open(path, encoding="utf-8") as f:
            yield (path, f.read())


def format_entry(entry):
    # type: (Tuple[str, str]) -> Iterator[str]
    path, text = entry
    return chain((format_header(path),), ensure_trailing_newline(text))


def format_header(path):
    # type: (str) -> str
    return "# source: {0!r}".format(path)


def ensure_trailing_newline(text):
    # type: (str) -> Iterable[str]
    if text.endswith("\n"):
        return (text,)
    return (text, "\n")


@click.group()
def cli():
    pass


@cli.command()
def install():
    generate()

    for dotfiles in all_dotfiles():
        print(f"Stowing {dotfiles.path}")
        stow("--dir", dotfiles.path, "--target", HOME, *dotfiles.package_names)


@cli.command()
def uninstall():
    for dotfiles in all_dotfiles():
        print(f"Unstowing {dotfiles.path}")
        stow("--dir", dotfiles.path, "--target", HOME, "-D", *dotfiles.package_names)


def generate():
    build_packages()
    combine_files()
    generate_stow_local_ignore()


@cli.command()
def check():
    generate()

    for dotfiles in all_dotfiles():
        print(f"Testing stow for {dotfiles.path}")
        stow(
            "--dir",
            dotfiles.path,
            "--target",
            HOME,
            "-v",
            "--simulate",
            *dotfiles.package_names,
        )


if __name__ == "__main__":
    cli()
