# emacs

## spacemacs

Setup with

```
git clone git@github.com:syl20bnr/spacemacs.git ~/.emacs.d
cd ~/.emacs.d && git checkout develop
```

Update with

```
cd ~/.emacs.d && git pull origin develop
```

then in emacs, execute `SPC f e D` to diff the template with the
customized file and pull in any new parts.

## doom

After the repository is initially cloned and deployed, start a new shell
and run `doom install`.

### initialization

NOTE: doom does not respect the `-y` command-line argument (at this time)

```
# By default, straight.el will delegate to our lockfile
YES=1 doom install --no-env --no-config
# Revert auto-upgraded melpa, emacsmirror-mirror,
doom thaw-packages
doom sync
```

To check that the packages are the same versions, run

```
doom freeze-packages
```

and confirm that `~/.emacs.d-doom-.local/straight/versions/default.el` was
not changed.

### adding packages

Add new package declarations to `.doom.d/package.el` and run `doom sync`.

Once happy with the results, run

```
doom freeze-packages
```

to update `default.el`, then commit it along with `.doom.d/package.el`.

### revert packages

The repository tracks the doom version and emacs package versions together.

Check out the doom submodule, `package.el`, configuration and `default.el` that correspond to the
desired, then

```
doom thaw-packages
doom sync
```

#### troubleshooting

##### Certificate verification failed

Error will say "The package manager threw an error", and the latest error output is something like

```
$ git fetch origin
fatal: unable to access 'https://package.org/git-repo-url': server certificate verification failed. CAfile: /etc/ssl/certs/ca-certificates.crt CRLfile: none
```

To mitigate, update system CA certificates package and try again.

For example, to update on Ubuntu: `apt install ca-certificates`

##### Failed to get unambiguous name

If the main branch name has been changed for a repo that's already cloned (as
of the version of straight.el I'm using), then `doom thaw-packages` may emit an
error like

```
In repository "repo-name", failed to get status:

Failed to get unambiguous name for "main"
fatal: Needed a single revision
```

When this happens, abort (respond to the prompt with 1), then navigate to
`~/.emacs.d-doom-.local/straight/repos` and move the existing repository folder
to a different directory, like `mv repo-name repo-name.old` and rerun `doom
thaw-packages`

May be related to https://github.com/radian-software/straight.el/issues/922

### upgrading packages

```
doom sync -u
```

Then follow "adding packages".

## org note setup

A basic org note will start with

```
#+SEQ_TODO: TODO(t) WAIT(w) | DONE(d)
#+ARCHIVE: ::* Archive
```

## organization

- `.doom.d/`: my personal doom emacs configuration
- `.emacs.d/`: (submodule) chemacs2 installation directory - permits switching out
  emacs profiles (like spacemacs/doom) more easily
- `.emacs.d-doom/`: (submodule) doom installation directory
- `.emacs.d-doom-.local/`: doom's local cache and `straight.el`-generated lock file
  get redirected to this folder - we only version control the lock file.
- `.emacs.d-private/`: my common utilities that were bit enough/independent enough
  to extract from my config.
- `.emacs.d-spacemacs/`: (submodule) spacemacs installation directory
- `.emacs-profiles.el`: configuration for chemacs2
- `.profile.d/`: shell init overrides
- `.spacemacs`: my personal spacemacs configuration (mostly deprecated)
