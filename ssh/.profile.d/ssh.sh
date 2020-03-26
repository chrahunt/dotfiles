global_ssh_auth_sock="$HOME/.ssh/auth_sock"

if [ -z "$SSH_AUTH_SOCK" -a -n "$WSL_SSH_AUTH_SOCK" ]; then
    # On WSL, symlink to the value set in the user environment.
    ln -sf "$WSL_SSH_AUTH_SOCK" "$global_ssh_auth_sock"
    export SSH_AUTH_SOCK="$global_ssh_auth_sock"
elif [ -z "$SSH_AUTH_SOCK" ]; then
    # In general, trust that we've set this up.
    export SSH_AUTH_SOCK="$global_ssh_auth_sock"
elif [ "$SSH_AUTH_SOCK" = "$global_ssh_auth_sock" ]; then
    :
elif [ -n "$SSH_TTY" ]; then
    # For interactive sessions, assume this latest connection is the one
    # that we want to keep.
    tty_ssh_auth_sock="$global_ssh_auth_sock.${SSH_TTY##*/}"
    # Symlink from a TTY-specific file to the original socket file.
    ln -sf "$SSH_AUTH_SOCK" "$tty_ssh_auth_sock"
    # Symlink from the global file to the TTY-specific file.
    ln -sf "$tty_ssh_auth_sock" "$global_ssh_auth_sock"
    # Set the global value.
    export SSH_AUTH_SOCK="$global_ssh_auth_sock"
fi
# Otherwise, we're non-interactive and not in WSL, so leave the original value.
