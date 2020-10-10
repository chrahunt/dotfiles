import nox


@nox.session(python=["3.8"], reuse_venv=True)
def build(session):
    session.install(".")


@nox.session(python=["3.8"], reuse_venv=True)
def test(session):
    session.install(".[test]")
    session.run("pytest", *session.posargs)
