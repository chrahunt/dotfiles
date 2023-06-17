;;; frame-prefix --- Easily set prefix on frame title

;; Author: Chris Hunt
;; Version: 1.0
;; Package-Requires: ((frame-local))

(require 'frame-local)

(defun frame-prefix/set (value)
  (interactive "sPrefix: ")
  (frame-local-set 'frame-prefix/frame-prefix-value value))

(defun frame-prefix/reset ()
  (interactive)
  (frame-local-set 'frame-prefix/frame-prefix-value nil))

(setq frame-prefix/common-suffix "%b - Emacs")

(defun frame-prefix/-get-frame-title ()
  (let* ((frame-prefix (frame-local-get 'frame-prefix/frame-prefix-value)))
    (if frame-prefix
      (concat "[" frame-prefix "] " frame-prefix/common-suffix)
      frame-prefix/common-suffix)))

(defun frame-prefix/register-frame-title-format ()
  (setq frame-title-format
        (list '(:eval (frame-prefix/-get-frame-title)))))

(provide 'frame-prefix)

;;; frame-prefix.el ends here
