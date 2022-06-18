;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; Provides yaml highlighting in src blocks in org
(package! yaml-mode)
;; Auto-commit files on save
(package! git-auto-commit-mode)
;; Extended task dependencies
(package! org-edna)
;; org-babel rust support
(package! rustic)
;; org-babel jq support
(package! jq-mode)
;; Spaced repetition in org mode
(package! org-drill)
;; QOL improvements for fcitx input
(package! fcitx)

;; Local packages
;; For each of these, right now I'm directing to my spacemacs layer
;; package directory, until I'm ready to switch completely.
;; Using `(use-package <name> :load-path "...")' ensures that these do not
;; show up in straight.el's lockfile, which is important since:
;; 1. it embeds the full path (and I use different home directories on different machines)
;; 2. it records the commit prior to the one for the lockfile, and I'm not sure what behavior
;;    that would have

;; Fixes for org-id-based org links
(use-package ol-org-id
  :defer
  :load-path "../.emacs.d-private/chrahunt/local/ol-org-id")

;; Nicer screenshot capture/image pasting experience
(use-package org-download-z
  :defer
  :load-path "../.emacs.d-private/chrahunt/local/org-download-z")

;; Clone indirect buffer in other frame
(use-package cibof
  :defer
  :load-path "../.emacs.d-private/chrahunt/local/cibof")

;; In-buffer capture template definitions
(use-package org-in-buffer-capture-templates
  :defer
  :load-path "../.emacs.d-private/chrahunt/local/org-in-buffer-capture-templates")

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; Workarounds
;; gitconfig-mode fails installation: https://github.com/hlissner/doom-emacs/issues/5667#issuecomment-948229579
;; can be removed after updating doom to a version that includes
;; https://github.com/hlissner/doom-emacs/pull/5665
(package! gitconfig-mode
      :recipe (:host github :repo "magit/git-modes"
             :files ("gitconfig-mode.el")))
(package! gitignore-mode
      :recipe (:host github :repo "magit/git-modes"
             :files ("gitignore-mode.el")))

;; org-contrib's default host seems to have an expired certificate
;; see https://github.com/hlissner/doom-emacs/issues/5655
(package! org-contrib
  :recipe (:host github :repo "emacsmirror/org-contrib"))
