import json
from pathlib import Path
from typing import Any, List, Union


def jsonl_loads(text: Union[bytes, str]) -> List[Any]:
    if isinstance(text, bytes):
        text = text.decode("utf-8")
    lines = text.splitlines()
    lines = [l.strip() for l in lines]
    lines = [l for l in lines if l]
    return [json.loads(l) for l in lines]


def make_tree(spec: List[str], base_dir: Path) -> None:
    for path in spec:
        full_path = base_dir / path
        if path.endswith("/"):
            full_path.mkdir(exist_ok=True, parents=True)
        else:
            full_path.parent.mkdir(exist_ok=True, parents=True)
            full_path.touch()


def expected_restic_output(paths, tmp_path):
    expected = {str(tmp_path)}
    # Restic doesn't include the root directory in its output, but
    # does include every parent directory.
    expected.update(str(p) for p in tmp_path.parents if str(p) != "/")
    for p in paths:
        new_path = tmp_path / p
        expected.add(str(new_path))
        for parent in new_path.parents:
            if parent == tmp_path:
                break
            expected.add(str(parent))
    return expected
