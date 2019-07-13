.PHONY: all stow unstow test generated

all: stow

stow: generated
	stow */

unstow:
	stow -D */

test: generated
	stow -v --simulate */

generated:
	./.dotfiles/combine-files .editrc.d editline/.editrc
	./.dotfiles/combine-files .gitignore_global.d git/.gitignore_global
	./.dotfiles/make-stow-local-ignore
