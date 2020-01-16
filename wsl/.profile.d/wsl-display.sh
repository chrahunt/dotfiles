# Assume that an X server has been started already.
if [ -z "$IS_WSL" ]; then
    return
fi
export DISPLAY=:0.0
