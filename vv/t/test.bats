load "$BATS_TEST_DIRNAME/../.profile.d/vv.sh"

mkdtemp() {
    _mkdtemp__d="$BATS_TMPDIR/$BATS_TEST_NAME"
    mkdir -p "$_mkdtemp__d"
    echo "$_mkdtemp__d"
}

python_mock() {
    _python_mock__called="$(python_mock_get_called)"
    python_mock_set_called "$((_python_mock__called + 1))"
}

python_mock_get_called() {
    _python_mock__called_f="$tmpdir/.python_mock_called"
    if [ -f "$_python_mock__called_f" ]; then
        cat "$_python_mock__called_f"
    else
        printf 0
    fi
}

python_mock_set_called() {
    _python_mock__called_f="$tmpdir/.python_mock_called"
    printf "%d" "$1">"$_python_mock__called_f"
}

python_mock_reset() {
    python_mock_set_called 0
}

setup() {
    tmpdir="$(mkdtemp)"
    VV_HOME="$tmpdir/vv_home"
    VV_PYTHON=python_mock
    python_mock_reset
}

teardown() {
    :
}

var_value() {
    eval "printf '%s' \"\$$1\""
}

pprint() {
    _pprint__name="$1"
    _pprint__value="$(var_value "$_pprint__name")"
    printf "%s = '%s'\n" "$_pprint__name" "$_pprint__value"
}

assert_eq() {
    _eq__v1="$1"
    _eq__v2="$2"
    pprint "$_eq__v1"
    pprint "$_eq__v2"
    [ "$(var_value "$_eq__v1")" = "$(var_value "$_eq__v2")" ]
}

@test "_vv_config_setitem with nonexistent file" {
    f="$tmpdir/f"
    [ ! -f "$f" ]
    run _vv_config_setitem "$f" A 1
    contents="$(cat "$f")"
    expected="$(printf "A=1\n")"
    assert_eq contents expected
}

@test "_vv_config_setitem with nonexistent key" {
    f="$tmpdir/f"
    [ ! -f "$f" ]
    echo "B=2" >"$f"
    run _vv_config_setitem "$f" A 1
    contents="$(cat "$f")"
    expected="$(printf "A=1\nB=2\n")"
    assert_eq contents expected
}

@test "_vv_config_setitem with existing key" {
    f="$tmpdir/f"
    [ ! -f "$f" ]
    printf "A=1\nB=2" >"$f"
    run _vv_config_setitem "$f" A 2
    contents="$(cat "$f")"
    expected="$(printf "A=2\nB=2\n")"
    assert_eq contents expected
}

@test "_vv_config_getitem with nonexistent file" {
    f="$tmpdir/f"
    [ ! -f "$f" ]
    run _vv_config_getitem "$f" A
    [ "$status" -eq 1 ]
}

@test "_vv_config_getitem with nonexistent key" {
    f="$tmpdir/f"
    [ ! -f "$f" ]
    echo "B=2" >"$f"
    run _vv_config_getitem "$f" A
    [ "$status" -eq 1 ]
    [ "$output" = "" ]
}

@test "_vv_config_getitem with existing key" {
    f="$tmpdir/f"
    [ ! -f "$f" ]
    printf "A=1\nB=2" >"$f"
    run _vv_config_getitem "$f" A
    [ "$status" -eq 0 ]
    [ "$output" = "1" ]
    run _vv_config_getitem "$f" B
    [ "$status" -eq 0 ]
    [ "$output" = "2" ]
}

@test "_vv_find_vvconfig with nonexistent config" {
    d="$tmpdir/d"
    mkdir -p "$d"
    cd "$d"
    run _vv_find_vvconfig
    [ "$status" -eq 1 ]
    [ -z "$output" ]
}

@test "_vv_find_vvconfig vvconfig in cwd" {
    d="$tmpdir/d"
    mkdir -p "$d"
    cd "$d"
    touch "$VV_CONFIG_NAME"
    run _vv_find_vvconfig
    expected="$d/$VV_CONFIG_NAME"
    assert_eq output expected
}

@test "_vv_find_vvconfig vvconfig in parent" {
    d="$tmpdir/d"
    d2="$d/d"
    mkdir -p "$d2"
    cd "$d2"
    expected="$d/$VV_CONFIG_NAME"
    touch "$expected"
    run _vv_find_vvconfig
    assert_eq output expected
}

@test "vv activate without existing venv creates venv" {
    d="$tmpdir/d"
    mkdir -p "$d"
    cd "$d"
    run vv activate
    echo "Output: $output"
    mock_called="$(python_mock_get_called)"
    expected_called=1
    assert_eq mock_called expected_called
    [ "$status" -eq 0 ]
}

@test "vv activate without existing vvconfig writes vvconfig" {
    d="$tmpdir/d"
    mkdir -p "$d"
    cd "$d"
    run vv activate
    [ "$status" -eq 0 ]
    [ -f "$VV_CONFIG_NAME" ]
    echo "Contents: $(cat "$VV_CONFIG_NAME")"
    grep "^$VV_CONFIG_KEY_DIR=" "$VV_CONFIG_NAME"
}


@test "vv activate uses existing venv" {
    fake_venv="$tmpdir/venv"
    mkdir -p "$fake_venv/bin"
    echo "touch $tmpdir/.activated" >"$fake_venv/bin/activate"
    d="$tmpdir/d"
    mkdir -p "$d"
    cd "$d"
    printf "$VV_CONFIG_KEY_DIR=$fake_venv\n" >"$VV_CONFIG_NAME"
    run vv activate
    echo "Output: $output"
    [ "$status" -eq 0 ]
    [ -f "$tmpdir/.activated" ]
}
