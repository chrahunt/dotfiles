(defconst chrahunt-packages
  '(
    ;; This prevents C-backspace from deleting words when at the beginning of
    ;; the line (since clean-aindent--inside-indentp is true). I use C-d and <<
    ;; anyway for dedenting in command/insert mode, so it's not useful.
    (clean-aindent-mode :excluded t)
    ;; Provides "vim-sneak"-behavior: s<2 characters> jumps to the next
    ;; instance of those two characters in the buffer.
    evil-snipe
    org
    ;; org-bullets has several issues:
    ;; - doesn't respect org-hide font face: https://github.com/sabof/org-bullets/pull/19
    ;; - doesn't work when switching theme: https://github.com/syl20bnr/spacemacs/issues/4688
    (org-bullets :excluded t)
    (org-in-buffer-capture-templates :location local)
    (org-download-z :location local)
    (use-browser-envvar :location local)
  )
)

(defun chrahunt/init-evil-snipe ()
  (use-package evil-snipe
    :config
    ;; Enable evil-snipe in normal mode.
    (evil-snipe-mode +1)
    ;; Search the rest of the buffer for the entered keys, not just the current
    ;; line.
    (setq evil-snipe-scope 'buffer)
    (setq evil-snipe-repeat-scope 'buffer)
    ;; Don't highlight all matches.
    (setq evil-snipe-enable-incremental-highlight nil)
    ;; I already use ';' to repeat search, so no need to support 's' to
    ;; repeat search.
    (setq evil-snipe-repeat-keys nil)
  )
)

(defun chrahunt/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    (chrahunt/fix-org-store-link)
    (chrahunt/fix-org-archive-subtree)
    :post-config
    ;; When `org-store-link` is executed, create an :ID: property for it and
    ;; store that, instead of using the headline. This makes it easier to move
    ;; nodes around without breaking links. Do not do it for auto-generated
    ;; links, as used for `%a` in capture templates, since I don't really
    ;; use those too much.
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
    ;; By default, org-clock functions only store the clock history in-memory.
    ;; Save it to disk instead, so it can be used after emacs restart.
    ;; I don't want to auto-resume tasks after kill, so use 'history instead of
    ;; t.
    (setq org-clock-persist 'history)
    ;; Required for org-clock-persist to take effect.
    (org-clock-persistence-insinuate)
    ;; By default this is 5. 35 is the suggested max so the org-clock-goto
    ;; output matches the number of available letters/numbers.
    (setq org-clock-history-length 35)
    ;; Keybinding: *C*lock > *l*ist
    (spacemacs/set-leader-keys-for-major-mode
      'org-mode "Cl" (lambda () (interactive) (org-clock-goto t)))
    ; Sets text for the above in helm selector.
    (which-key-add-major-mode-key-based-replacements
      'org-mode ",Cl" "list recent")
    ;; I mostly refile tasks under "Projects" * "tasks", these helpers make that easier.
    (defun chrahunt/matches-task-heading ()
      (let ((outline-path (org-get-outline-path t t)))
        (and
          (string= (car outline-path) "Projects")
          (string= (car (last outline-path)) "tasks")
        )
      )
    )
    (defun chrahunt/org-refile-to-project ()
      (interactive)
      (let ((org-refile-target-verify-function #'chrahunt/matches-task-heading))
        (org-refile)
      )
    )
    ;; Major-mode keybinding setup
    (spacemacs/declare-prefix-for-mode 'org-mode "o" "custom")
    ;; Keybinding: *o* > *r*efile
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "or" #'chrahunt/org-refile-to-project)
  )
)

(defun chrahunt/init-org-download-z ()
  (use-package org-download-z
    :config
    ;; *i*nsert > *D*ownload > *p*aste
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "iDp" #'org-download-z-paste)
  )
)

(defun chrahunt/init-org-in-buffer-capture-templates ()
  (use-package org-in-buffer-capture-templates
    :config
    ;; *C*apture > *b*uffer
    (spacemacs/set-leader-keys "Cb" #'org-capture-from-buffer)
  )
)

(defun chrahunt/init-use-browser-envvar ()
  (use-package use-browser-envvar)
)
