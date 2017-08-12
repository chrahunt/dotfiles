#!/bin/sh
# Common aliases/settings/functions
set -o vi

alias g=grep
alias ..='cd ..'
alias ...='cd .. && cd ..'
alias ....='cd .. && cd .. && cd ..'
alias .....='cd .. && cd .. && cd .. && cd ..'

# Interactive functions.
# Re-source profile.
alias pr=". ~/.profile"

# Return code.
rc() {
    echo $?
}

alias t=tmux
if which tmux-next >/dev/null 2>&1; then
    alias tmux=tmux-next
fi
