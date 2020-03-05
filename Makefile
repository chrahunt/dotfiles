.PHONY: all stow unstow test generated

all: stow

stow: generated
	stow --dir "$$PWD" --target "$$HOME" */

unstow:
	stow --dir "$$PWD" --target "$$HOME" -D */

test: generated
	stow --dir "$$PWD" --target "$$HOME" -v --simulate */

generated:
	./.dotfiles/combine-files-py --match-dir-name .editrc.d . > editline/.editrc
	./.dotfiles/combine-files-py --match-dir-name .gitignore_global.d . > git/.gitignore_global
	./.dotfiles/make-stow-local-ignore
