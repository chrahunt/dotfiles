.PHONY: all install unstow check generated

all: install

install: generated
	@for d in ~/.dotfiles.d/*/; do \
	    echo "Stowing $$d"; \
	    cd "$$d"; \
	    stow --dir "$$PWD" --target "$$HOME" */; \
	    cd - >/dev/null; \
	done

unstow:
	for d in ~/.dotfiles.d/*; do \
	    echo "Unstowing $$d"; \
	    cd "$$d"; \
	    stow --dir "$$PWD" --target "$$HOME" -D */; \
	    cd - >/dev/null; \
	done

check: generated
	for d in ~/.dotfiles.d/*; do \
	    echo "Testing $$d"; \
	    cd "$$d"; \
	    stow --dir "$$PWD" --target "$$HOME" -v --simulate */; \
	    cd - >/dev/null; \
	done
	$(MAKE) -C emacs

generated:
	./.dotfiles/combine-files-py --match-dir-name .editrc.d ~/.dotfiles.d/* > editline/.editrc
	./.dotfiles/combine-files-py --match-dir-name .gitignore_global.d ~/.dotfiles.d/* > git/.gitignore_global
	./.dotfiles/make-stow-local-ignore ~/.dotfiles.d/*
