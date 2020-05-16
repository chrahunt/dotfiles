(defun chrahunt/fix-org-archive-subtree ()
  (with-eval-after-load 'org-archive
    ;; By default, calling `org-archive-subtree` to archive to the current file
    ;; results in all nodes being expanded, since internally it calls
    ;; `org-show-all`. We wrap it with `org-save-outline-visibility` to preserve
    ;; the node visibility.
    (advice-add 'org-archive-subtree :around
      (lambda (fn &rest args)
        ;; Set USE-MARKERS t, so that edits to the current file
        ;; don't mess up our cursor reset.
        (org-save-outline-visibility t (apply fn args))
      )
    )
  )
)

(defun chrahunt/fix-org-store-link ()
  (with-eval-after-load 'ol
    ;; If not explicitly required, then `org-store-link` will ignore the
    ;; setting for `org-id-link-to-org-use-id`.
    (require 'org-id)
  )
)
