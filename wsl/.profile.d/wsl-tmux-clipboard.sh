if [ -n "$IS_WSL" ]; then
    return
fi

export TMUX_COPY_COMMAND="/mnt/c/Windows/System32/clip.exe"
