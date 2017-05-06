all: stow

stow:
	stow */

unstow:
	stow -D */

test:
	stow -v --simulate */
