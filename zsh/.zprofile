# Delegate to ~/.profile
[ -e "$HOME/.profile" ] && emulate sh -c "
IN_ZPROFILE=1
source '$HOME/.profile'
"
