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

PATH="$HOME/bin:$PATH"
