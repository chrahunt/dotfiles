# backup

Wrapper around restic.

## getting restic

This project assumes the use of the restic at
https://github.com/chrahunt/restic/tree/pr-3200 which is the latest master
and restic/restic#3200, supporting:

1. `restic --files-from-raw`
2. `restic --set-path`

## configuration

Configuration lives in a file. The path to the configuration must be passed
on the command-line (`-f`). Configuration can be YAML or JSON, and can contain:

- `base_directory` (string, required): top-level directory to consider when
  backing up files.
- `env_command` (string, optional): shell command that, when executed, must
  produce a valid JSON object. The resulting key/value pairs are substituted
  into the values provided in `env`, using Python's format-string syntax. Any
  non-zero exit code fails the backup process.
- `options` (object, optional): contained key/value pairs are serialized into
  `-o k=v` and passed to `restic backup`
- `env` (object, optional): key/value pairs used as the environment when
  executing restic. Prior to being set in the environment, values are
  substituted like typical Python format strings, with items provided by
  `env_command` (plus `hostname`, which corresponds to `socket.gethostname()`)
- `exclude_dirs` (array, optional): list of paths. May contain:
  - Absolute paths (leading `/`, permitted to contain `/`) - directories
    matching this path, interpreted relative to `base_directory` will not be
    backed up
  - Single path names (no `/` permitted) - directories with this name will not
    be backed up to `base_directory`. Relative paths match any full directory name.

In addition to the `exclude_dirs` option above, directories containing a file named
`.nobackup` are excluded from backup.

### examples

#### Hard-coded secrets

`$HOME/.backup/config.json` has

```
{
    "base_directory": "/home/user",
    "env_command": "cat \"$HOME/secrets/passwords.json\"",
    "options": {
        "b2.connections": "25"
    },
    "env": {
        "B2_ACCOUNT_ID": "{b2_account_id}",
        "B2_ACCOUNT_KEY": "{b2_account_key}",
        "RESTIC_REPOSITORY": "b2:{b2_bucket}:home",
        "RESTIC_PASSWORD": "{restic_password}"
    }
}
```

where `$HOME/secrets/passwords.json` has

```
{
    "b2_account_id": "aaaaaaaaaaa",
    "b2_account_key": "ABC+/",
    "b2_bucket": "my-backup",
    "restic_password": "password"
}
```

#### Some other secrets mechanism

`$HOME/.backup/config.json` has

```
{
    "base_directory": "/home/user",
    "env_command": "\"$HOME/bin/get-backup-secrets\"",
    "env": {
        "AWS_ACCESS_KEY_ID": "{AWS_ACCESS_KEY_ID}",
        "AWS_SECRET_ACCESS_KEY": "{AWS_SECRET_ACCESS_KEY}",
        "AWS_SESSION_TOKEN": "{AWS_SESSION_TOKEN}",
        "AWS_DEFAULT_REGION": "{AWS_DEFAULT_REGION}"
    }
}
```

where `$HOME/bin/get-backup-secrets` does something and prints

```
{
    "AWS_ACCESS_KEY_ID": "...",
    "AWS_SECRET_ACCESS_KEY": "...",
    "AWS_SESSION_TOKEN": "...",
    "AWS_DEFAULT_REGION": "..."
}
```
