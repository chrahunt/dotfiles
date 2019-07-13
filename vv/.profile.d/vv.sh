# Provides a Python venv helper.

VV_HOME=~/.virtualenvs
VV_CONFIG_NAME=.vvconfig
VV_PYTHON="${VV_PYTHON:-python}"
VV_CONFIG_KEY_DIR=DIR

vv() {
    _vv__cmd="$1"
    shift
    if [ -z "$_vv__cmd" ]; then
        _vv__cmd=activate
    fi
    case "$_vv__cmd" in
        activate) vv_activate "$@";;
        *) vv_usage "$@";
    esac
    return $?
}

_vv_log() {
    _vv_log__name="$1"
    shift
    printf "\033[0;32m[%s]\033[0m " "$_vv_log__name"
    echo "$@"
}

_vv_error() {
    _vv_error__name="$1"
    shift
    printf "\033[0;31m[%s]\033[0m " "$_vv_error__name"
    echo "$@"
}

vv_activate() {
    _vv_activate__vvconfig="$(_vv_find_vvconfig)"
    if [ "$?" = "0" ]; then
        _vv_activate__dir="$(_vv_config_getitem "$_vv_activate__vvconfig" "$VV_CONFIG_KEY_DIR")"
        if [ -n "$_vv_activate__dir" ]; then
            _vv_log activate "Activating $_vv_activate__vvconfig_dir"
            . "$_vv_activate__dir/bin/activate"
            return 0
        fi
    fi

    _vv_log activate "No venv found, creating..."
    _vv_activate__name="${PWD##*/}"
    _vv_activate__dir="$(vv_create "$_vv_activate__name")"
    if [ "$?" = "0" ]; then
        _vv_config_setitem "$VV_CONFIG_NAME" "$VV_CONFIG_KEY_DIR" "$_vv_activate__dir"
        . "$_vv_activate__dir/bin/activate"
        return 0
    fi

    _vv_error activate "Could not create venv"
    return 1
}

vv_create() {
    _vv_create__name="$1"
    shift
    _vv_create__tmpdir="$(_vv_get_tmpdir)"
    "$VV_PYTHON" -m venv --prompt "$_vv_create__name" "$_vv_create__tmpdir"
    echo "$_vv_create__tmpdir"
    return $?
}

_vv_get_tmpdir() {
    mkdir -p "$VV_HOME"
    mktemp -d -p "$VV_HOME"
    return $?
}

_vv_config_getitem() {
    _vv_config_getitem__self="$1"
    _vv_config_getitem__key="$2"
    if [ ! -f "$_vv_config_getitem__self" ]; then
        return 1
    fi
    _vv_config_getitem__entry="$(
        cat "$_vv_config_getitem__self" |
        grep "^$_vv_config_getitem__key="
    )"
    if [ "$?" -ne 0 ]; then
        return 1
    fi
    printf "%s" "${_vv_config_getitem__entry#*=}"
}

_vv_config_setitem() {
    _vv_config_setitem__self="$1"
    _vv_config_setitem__key="$2"
    _vv_config_setitem__value="$3"
    if [ -f "$_vv_config_setitem__self" ]; then
        _vv_config_setitem__contents="$(cat "$_vv_config_setitem__self")"
    else
        _vv_config_setitem__contents="$(printf "\n")"
    fi

    _vv_config_setitem__entry="$_vv_config_setitem__key=$_vv_config_setitem__value"
    if [ -z "$_vv_config_setitem__contents" ]; then
        echo "$_vv_config_setitem__entry" >"$_vv_config_setitem__self"
        return 0
    fi
    _vv_config_setitem__contents="$(
        printf "%s" "$_vv_config_setitem__contents" |
        grep -v "^$_vv_config_setitem__key="
    )"
    if [ -z "$_vv_config_setitem__contents" ]; then
        echo "$_vv_config_setitem__entry" >"$_vv_config_setitem__self"
        return 0
    fi
    printf "%s\n%s\n" "$_vv_config_setitem__contents" "$_vv_config_setitem__entry" \
        | sort \
        >"$_vv_config_setitem__self"
}

# Given a directory (or PWD), find the closest .vvconfig in any parent directory.
_vv_find_vvconfig() {
    _vv_find_vvconfig__dir="${1:-$PWD}"
    _vv_find_vvconfig__last=""
    while true; do
        _vv_find_vvconfig__test="$_vv_find_vvconfig__dir/$VV_CONFIG_NAME"
        if [ "$_vv_find_vvconfig__test" = "$_vv_find_vvconfig__last" ]; then
            return 1
        fi
        _vv_find_vvconfig__last="$_vv_find_vvconfig__test"

        if [ -f "$_vv_find_vvconfig__test" ]; then
            printf "%s" "$_vv_find_vvconfig__test"
            return 0
        fi

        _vv_find_vvconfig__dir="${_vv_find_vvconfig__dir%/*}"
    done
}
