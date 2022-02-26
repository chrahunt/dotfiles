import pytest


@pytest.fixture
def ok():
    """Accumulate expected outputs defined alongside other inputs.

    Example:
        inputs = [
            0 > ok,
            1,
            2 > ok,
        ]
        expected = list(ok)
        assert even_numbers(inputs) == expected
    """
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
