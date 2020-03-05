# By default zsh behaves like vi and doesn't allow deleting further
# back than the latest insert, this fixes it.
zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

# Since Esc is used as a prefix for determining multi-key input,
# zsh waits by default. This causes a noticeable delay when going
# from insert to command mode, so reduce the delay.
# 10ms.
KEYTIMEOUT=1

# Make `v` in command mode open EDITOR, like in bash/ksh.
autoload -z edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

alias ..='cd ..'
alias ...='cd .. && cd ..'
alias ....='cd .. && cd .. && cd ..'
alias .....='cd .. && cd .. && cd .. && cd ..'
alias ......='cd .. && cd .. && cd .. && cd .. && cd ..'
alias .......='cd .. && cd .. && cd .. && cd .. && cd .. && cd ..'
