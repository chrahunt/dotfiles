# separate config files
if [ -d "$HOME/.profile.d" ]; then
  for i in $HOME/.profile.d/*.sh; do
    test ! -z "$DEBUG_SHELL" && echo "Sourcing $i"
    if [ -r "$i" ]; then
      . "$i"
    fi
  done
  unset i
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"
