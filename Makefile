.PHONY: all stow unstow test generated

all: stow

stow: generated
	stow --dir "$$PWD" --target "$$HOME" */

unstow:
	stow --dir "$$PWD" --target "$$HOME" -D */

test: generated
	stow --dir "$$PWD" --target "$$HOME" -v --simulate */

generated:
	./.dotfiles/combine-files .editrc.d editline/.editrc
	./.dotfiles/combine-files .gitignore_global.d git/.gitignore_global
	./.dotfiles/make-stow-local-ignore
