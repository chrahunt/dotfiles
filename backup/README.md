# backup

Wrapper around restic. Generates and escapes a file list.

Currently the top-level always-excluded paths are hard-coded, and directories
containing a file named `.nobackup` are ignored.

## getting restic

This project assumes the use of the restic at
https://github.com/chrahunt/restic/tree/pr-3200 which is the latest master
and restic/restic#3200, supporting:

1. `backup --files-from-verbatim`
2. `backup --set-path`

## configuration

Configuration lives in `$HOME/.backup/config.json` and must contain:

- `base_directory` (string):
- `env_command` (string): shell command that, when executed, must produce a
  valid JSON object. Each key/value pair is used in the environment for restic
  commands
- `options` (object): contained key/value pairs are serialized into `-o k=v`
  and passed to `restic backup`

### examples

#### Hard-coded secrets

`$HOME/.backup/config.json` has

```
{
    "base_directory": "/home/user",
    "env_command": "cat \"$HOME/secrets/passwords.json\"",
    "options": {
        "b2.connections": "25"
    }
}
```

where `$HOME/secrets/passwords.json` has

```
{
    "B2_ACCOUNT_ID": "aaaaaaaaaaa",
    "B2_ACCOUNT_KEY": "ABC+/",
    "RESTIC_REPOSITORY": "b2:my-backup",
    "RESTIC_PASSWORD": "password"
}
```

#### Some other secrets mechanism

`$HOME/.backup/config.json` has

```
{
    "base_directory": "/home/user",
    "env_command": "\"$HOME/bin/get-backup-secrets\""
}
```

where `$HOME/bin/get-backup-secrets` does something and returns

```
{
    "AWS_ACCESS_KEY_ID": "...",
    "AWS_SECRET_ACCESS_KEY": "...",
    "AWS_SESSION_TOKEN": "...",
    "AWS_DEFAULT_REGION": "..."
}
```
