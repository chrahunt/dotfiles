import nox


@nox.session(python=["3.8"], reuse_venv=True)
def all(session):
    session.install("click", "nox")
    session.run("python", ".dotfiles/dotfiles", *session.posargs)


@nox.session(python=["3.8"], reuse_venv=True)
def lint(session):
    session.install("pre-commit")

    if session.posargs:
        args = session.posargs + ["--all-files"]
    else:
        args = ["--all-files", "--show-diff-on-failure"]

    session.run("pre-commit", "run", *args)
