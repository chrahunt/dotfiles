all: stow

stow:
	cat */.editrc.d/* > editline/.editrc
	stow */

unstow:
	stow -D */

test:
	stow -v --simulate */
