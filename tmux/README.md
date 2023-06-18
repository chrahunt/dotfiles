# tmux

This tmux configuration is parameterized in a few ways to support different
environments:

1. The `TMUX_COPY_COMMAND` environment variable should be set to a command
   which sets the system clipboard with contents passed on stdin.

## setup

Install "Tmux Plugin Manager" (https://github.com/tmux-plugins/tpm), by running

```
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```
