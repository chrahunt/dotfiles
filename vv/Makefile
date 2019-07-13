vv_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

test:
	docker run -it -v "$(vv_dir):/code" bats/bats:latest /code/t
