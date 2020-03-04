_cdf_prettyprint() {
    local first=1
    while IFS= read -r -d '' line; do
        if [ "$first" -eq "1" ]; then
            first=0
        else
            printf "\0"
        fi
        printf "%s" "${line##*/}"
    done
}

cdf() {
    local next
    local cmd
    local dir="${1:-$HOME}"
    while true; do
        # Enter - ok
        # ctrl-c - cancel
        # ctrl-d - done
        # ctrl-u - up
        # Find all directories in the current directory, following symlinks.
        next="$(
            find -L "$dir/" -maxdepth 1 -mindepth 1 -type d -print0 \
            | _cdf_prettyprint \
            | fzf --read0 --expect=ctrl-c,ctrl-d,ctrl-u --header="$dir"
        )"
        cmd="$(printf "%s" "$next" | head -1)"
        next="$(printf "%s" "$next" | tail -1)"
        # Get dirname for appending to dir.
        next="${next##*/}"
        case "$cmd" in
            ctrl-c) break;;
            ctrl-d) cd "$dir"; break;;
            ctrl-u) dir="${dir%/*}"; continue;;
            *) dir="$dir/$next";
        esac
    done
}
