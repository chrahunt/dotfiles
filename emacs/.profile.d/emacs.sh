if [ -n "$IS_WSL" ]; then
    # Lazy-start emacs server
    # --alternate-editor='' - daemon will be started if not already running
    # --create-frame - create a new window, don't attach
    # --no-wait - daemonize the gui process
    alias em="emacsclient26 --alternate-editor='' --create-frame --no-wait"
else
    # With native Ubuntu, using user-mode systemd-managed emacs server, just
    # connect to it.
    alias em="emacsclient -cn"
fi
