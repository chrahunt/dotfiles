# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# separate config files
if [ -d "$HOME/.profile.d" ]; then
  for i in "$HOME/.profile.d/*.sh"; do
    if [ -r "$i" ]; then
      . "$i"
    fi
  done
  unset i
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"
