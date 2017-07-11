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
