#!/bin/sh

script="$PWD/../combine-files-py"
d="$(mktemp -d)"
cd "$d"

mkdir -p d1/a d1/b d1/c
echo "text a" > d1/a/1
echo "text b" > d1/b/1
echo "text c" > d1/c/1

# $() trims trailing newline unconditionally, so add a character
# and remove it
result="$($script --match-file-name 1 d1; printf a)"
result="${result%a}"
expected="\
# source: 'd1/a/1'
text a

# source: 'd1/b/1'
text b

# source: 'd1/c/1'
text c
"
if [ "$expected" != "$result" ]; then
    printf "FAILED\n'''%s'''\ndoes not equal\n'''%s'''\n" "$result" "$expected"
    exit 1
fi
