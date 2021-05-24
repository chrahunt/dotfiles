"""Helper script to iterate over and run git-bark in each repository.
"""
import argparse
import json
import logging
import subprocess


logger = logging.getLogger(__name__)


def get_repo_worktrees(config_path: str):
    result = subprocess.run(
        ["backup", "-f", str(config_path), "ls-from-config"],
        stdout=subprocess.PIPE,
        check=True,
    )
    text = result.stdout.decode("utf-8")
    files = json.loads(text)
    repos = set()
    for path in files:
        if path.endswith("/.git"):
            repos.add(path[:-len("/.git")])
        else:
            try:
                i = path.index("/.git/")
            except ValueError:
                continue
            repos.add(path[:i])
    return sorted(repos)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", required=True)
    args = parser.parse_args()
    logging.basicConfig(level=logging.DEBUG)
    repo_worktrees = get_repo_worktrees(args.f)
    for d in repo_worktrees:
        logger.info("Backing up %r", d)
        subprocess.run(["git", "bark", "backup"], cwd=d)


if __name__ == "__main__":
    main()
