if command_exists vim; then
    export EDITOR=vim
elif command_exists vi; then
    export EDITOR=vi
fi
alias v="$EDITOR"
