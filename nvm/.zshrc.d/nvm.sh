# nvm complains if $HOME is a symlink, so resolve $HOME
# See https://stackoverflow.com/a/58559982
export NVM_DIR="$(readlink -f "$HOME/.nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
