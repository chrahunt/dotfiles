import pytest


@pytest.fixture
def ok():
    class Ok:
        def __init__(self):
            self.value = []

        def __lt__(self, v):
            self.value.append(v)
            return v

        def __iter__(self):
            v, self.value = self.value, []
            return iter(v)

    return Ok()
