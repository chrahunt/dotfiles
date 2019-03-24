# Check if a name exists in the current interactive session and is callable.
# Args:
#   (str) command - the command to search for
command_exists_i() {
    command -v "$1" > /dev/null 2>&1
}

# Check if a command corresponds to an executable.
# Bash-specific implementation.
# Args:
#   (str) command - the command to search for
command_exists() {
    type -P "$1" > /dev/null 2>&1
}

str_contains() {
    [ -z "${1##*$2*}" ]
}

env_prepend() {
    eval $1="$2:\$$1"
}

env_append() {
    eval $1="\$$1:$2"
}

path_filter() {
    PATH="${PATH#$1:}"
    PATH="${PATH%:$1}"
    PATH="${PATH//:$1:/:}"
}

path_prepend() {
    path_filter "$1"
    env_prepend PATH "$1"
}

path_append() {
    path_filter "$1"
    env_append PATH "$1"
}
