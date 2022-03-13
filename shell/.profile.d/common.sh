#!/bin/sh
# Common aliases/settings/functions
# When sourced from .zprofile in emulate mode, this traces
# "can't change option: vi", so don't do that.
[ -z "$ZSH_NAME" ] && set -o vi

alias ..='cd ..'
alias ...='cd .. && cd ..'
alias ....='cd .. && cd .. && cd ..'
alias .....='cd .. && cd .. && cd .. && cd ..'
alias ......='cd .. && cd .. && cd .. && cd .. && cd ..'
alias .......='cd .. && cd .. && cd .. && cd .. && cd .. && cd ..'
