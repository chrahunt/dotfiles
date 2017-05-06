if command vim > /dev/null 2>&1; then
    export EDITOR=vim
elif command vi > /dev/null 2>&1; then
    export EDITOR=vi
fi
