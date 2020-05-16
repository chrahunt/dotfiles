;;; org-in-buffer-capture-templates.el --- Buffer-specific capture templates.

;; Author: Chris Hunt
;; Version: 1.0
;; Package-Requires: ()

(defun org-babel-execute:org-capture-template (body params)
  "Evaluation function for src block evaluation"
  (interactive)
  (let*
    (
      ; Shadow any defaults so org-babel-execute:emacs-lisp returns
      ; the elisp value.
      (params (push '(:result-params . nil) params))
      (template (org-babel-execute:emacs-lisp body params))
      (template-arg (car template))
      (org-capture-templates `((,@template)))
    )
    (org-capture nil template-arg)
  )
)

;; Set major mode for our source blocks.
(add-to-list 'org-src-lang-modes '("org-capture-template" . emacs-lisp))

;;;###autoload
(defun org-capture-from-buffer ()
  (interactive)
  (let ((org-capture-templates '()))
    ; Eval all source blocks using our language and merge into org-capture-templates.
    (org-babel-map-executables nil
      (let*
        (
          (element (org-element-at-point))
          (language (org-element-property :language element))
          (value (org-element-property :value element))
        )
        (if (string= language "org-capture-template")
          (setq
            org-capture-templates
            (push (org-babel-execute:emacs-lisp value nil) org-capture-templates)
          )
        )
      )
    )
    (org-capture)
  )
)

(provide 'org-in-buffer-capture-templates)
