#!/bin/sh
if command_exists tmux-next; then
    _TMUX_BIN=tmux-next
    alias t=tmux-next
elif command_exists tmux; then
    _TMUX_BIN=tmux
    alias t=tmux
fi
export TMUX_CONFIG=~/.tmux.conf
export TMUX_COPY_COMMAND="xclip -in -selection clipboard"
