if [ -z "$SSH_AUTH_SOCK" ]; then
    # Set per README for pass-through to WSL.
    if [ -n "$WSL_SSH_AUTH_SOCK" ]; then
        export SSH_AUTH_SOCK="$WSL_SSH_AUTH_SOCK"
    fi
fi
