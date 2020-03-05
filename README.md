# dotfiles

Contains configuration files for applications I use regularly.

Uses [GNU Stow](https://www.gnu.org/software/stow/) to deploy into a
home directory.

## usage

Setup

    mkdir -p ~/.dotfiles.d
    ln -s $PWD ~/.dotfiles.d/main

Deploy

    make

See files changed

    make test

Add other dotfiles roots into `~/.dotfiles.d/`

## details

Goals:

1. Allow isolated configuration of apps and associated aliases/helpers,
   organized by directory
2. Where app 1 needs to apply configuration to app 2:
   1. if app 2 supports aggregating config (e.g. `.profile.d`), use that
   2. else, combine files together from each directory
3. Support multiple dotfile folder roots - so that configuration for one
   machine can live in different repositories with different levels of
   visibility (personal public vs private work-specific).

Scripts expect to see directories under `~/.dotfiles.d/`.

Scripts supporting the goals above are in `.dotfiles/`.

Global configuration is in `dotfiles/`.
