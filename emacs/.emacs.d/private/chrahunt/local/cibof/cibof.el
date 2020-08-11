;;; cibof.el --- Clone indirect buffers in another frame.

;; Author: Chris Hunt
;; Version: 1.0
;; Package-Requires: ()

;;;###autoload
(defun clone-indirect-buffer-other-frame (newname &optional norecord)
  "Like `clone-indirect-buffer' but display in another frame."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
         (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
           (read-buffer "Name of indirect buffer: " (current-buffer))))))
  (let* ((buf (clone-indirect-buffer newname nil norecord))
         ; We just want a basic frame, so disable perspective, which would
         ; otherwise cause the windows to be inherited.
         (persp-init-new-frame-behaviour-override (lambda (&rest args) nil))
         ; Use make-frame directly instead of `(pop-up-frames t)' around
         ; `clone-indirect-buffer', since the latter will engage more library
         ; callbacks (like purpose).
         (frame (make-frame)))
    (select-frame frame)
    (switch-to-buffer buf nil 'force-same-window)))

(provide 'cibof)

;;; cibof.el ends here
