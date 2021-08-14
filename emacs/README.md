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

When the repository is initially cloned and deployed, start a new shell
and run `doom install`.

To use doom-based configuration, run emacs with `--with-profile doom`.

## org note setup

A basic org note will start with

```
#+SEQ_TODO: TODO(t) WAIT(w) | DONE(d)
#+ARCHIVE: ::* Archive
```

## organization

`.spacemacs` includes layer `chrahunt`, in which I have my configuration.

Most of the package-specific settings are in `chrahunt/packages.el`,
which calls out to functions in `chrahunt/funcs.el` as needed.
