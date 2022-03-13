# Delegate to ~/.profile
[ -e "$HOME/.profile" ] && emulate sh -c "source '$HOME/.profile'"
