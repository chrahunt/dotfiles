# git

Use cases:

1. Machine/environment user-specific config -> put it in `~/.gitconfig.overrides`, git takes
   the last value encountered and that is included at the bottom of `~/.gitconfig`. Any repo-
   specific deviations from that should be set in `.git/config`.
