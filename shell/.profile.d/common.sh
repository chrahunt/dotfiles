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
