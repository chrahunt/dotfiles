_DOTFILES_SSH_AUTH_SOCK="$HOME/.ssh/auth_sock"
if [ -z "$SSH_AUTH_SOCK" ]; then
    # Set per README for pass-through to WSL.
    if [ -n "$WSL_SSH_AUTH_SOCK" ]; then
        export SSH_AUTH_SOCK="$WSL_SSH_AUTH_SOCK"
    elif [ -e "$_DOTFILES_SSH_AUTH_SOCK" ]; then
        export SSH_AUTH_SOCK="$_DOTFILES_SSH_AUTH_SOCK"
    fi
fi
