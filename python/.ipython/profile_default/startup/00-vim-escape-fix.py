"""
By default, prompt toolkit waits for 1 second after escape is pressed before
that is indicated in prompt state. This creates a bad, delayed-looking experience
when switching from insert mode to command/nav mode when using vi keys.

This avoids that by setting the max timeout from 1 second to 10 ms.

This will be available via the IPython config file from here:
https://github.com/ipython/ipython/pull/12588, check later if this is in stable.
"""


def _main():
    from IPython.core.getipython import get_ipython

    app = get_ipython().pt_app.app
    app.timeoutlen = 0.01
    app.ttimeoutlen = 0.01


# Keep namespace clean.
_main()
del _main
