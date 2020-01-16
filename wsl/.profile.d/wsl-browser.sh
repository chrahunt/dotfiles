# Configure BROWSER to invoke Firefox in Windows on WSL.
if [ -z "$IS_WSL" ]; then
    return
fi
export BROWSER="$HOME/bin/wsl-browser"
