if [ -d "$HOME/.zshrc.d" ]; then
  for i in $HOME/.zshrc.d/*.sh; do
    test ! -z "$DEBUG_SHELL" && echo "Sourcing (zsh) $i"
    if [ -r "$i" ]; then
      . "$i"
    fi
  done
  unset i
fi
