;;; ol-org-id.el --- Nice id links in indirect buffers.

;; Author: Chris Hunt
;; Version: 1.0
;; Package-Requires: ((org) (cibof))

(require 'ol)
(require 'org-id)

(defvar-local org-id-open-links-in-same-buffer nil
  "Open id-based links in the same buffer.")

(defun set-org-id-open-links-in-same-buffer ()
  "Helper function for setting buffer-local
`org-id-open-links-in-same-buffer'."
  (setq-local org-id-open-links-in-same-buffer t))

(defun ol-org-id--find-buffer-visiting-same-advice (buf)
  (let ((base-buf (buffer-base-buffer)))
    (if (eq base-buf buf)
        (current-buffer)
      buf)))

(defmacro with-advice-added (symbol where function &rest body)
  "Temporarily advise SYMBOL with FUNCTION according to WHERE while executing
BODY.

\(fn SYMBOL WHERE FUNCTION BODY...)"
  (let ((symbol-var (make-symbol "symbol-var"))
        (where-var (make-symbol "where-var"))
        (function-var (make-symbol "function-var")))
    ;; We assign to separate non-conflicting variables to only evaluate
    ;; our arguments once, in case they are not symbols/literals.
    `(let ((,symbol-var ,symbol)
           (,where-var ,where)
           (,function-var ,function))
       (advice-add ,symbol-var ,where-var ,function-var)
       (unwind-protect
           ,(macroexp-progn body)
         (advice-remove ,symbol-var ,function-var)))))

(defun ol-org-id--org-id-find-in-same-buffer (id &optional markerp)
  "`org-id-find' delegates to `find-buffer-visiting', which
retrieves the base buffer that contains the id. We customize the
behavior by returning the current buffer if the retrieved base buffer is the
same as this buffer's base."
  (with-advice-added 'find-buffer-visiting
                     :filter-return
                     #'ol-org-id--find-buffer-visiting-same-advice
    (org-id-find id markerp)))

(defun ol-org-id--get-current-id-link ()
  "If we're on an 'id'-type link, return it."
  (let ((context (org-element-lineage (org-element-context) '(link) t)))
    (when context
      (let* ((link (cadr context))
             (type (plist-get link :type)))
        (when (string= type "id")
          link)))))

(defun ol-org-id--indirect-internal-handler ()
  "Open id-based links in the same buffer, if `org-id-open-links-in-same-buffer'
is set."
  ;; Inspired by `org-open-at-point' and `org-id-open', which is set as the
  ;; default handler for "id"-type links.
  (let ((is-indirectp (buffer-base-buffer)))
    (when (and is-indirectp org-id-open-links-in-same-buffer)
      (let ((link (ol-org-id--get-current-id-link)))
        (when link
          ;; By default `org-id-open' (as used in `org-open-at-point')
          ;; does both the finding and the switching. We do the same here,
          ;; but only advising around `org-id-find', to reduce the amount
          ;; we're interfacing with code that could change.
          (let* ((path (plist-get link :path))
                 (m (ol-org-id--org-id-find-in-same-buffer path 'marker))
                 (buf (marker-buffer m)))
            ;; Our override is only relevant if the logic resulted in a buffer
            ;; that was the same as our current one. If not, then we'll return
            ;; nil and fall back to whatever was going to happen in
            ;; `org-open-at-point'.
            (when (eq buf (current-buffer))
              (goto-char m)
              (move-marker m nil)
              (org-show-context)
              ;; Terminates further link processing within `org-open-at-point'.
              t)))))))

;; Override default link handlers, conditional on our callback returning non-`nil'.
(add-hook 'org-open-at-point-functions #'ol-org-id--indirect-internal-handler)

(defmacro with-hook-added (hook fn &rest body)
  "Temporarily add FN to HOOK while executing BODY.

\(fn HOOK FN BODY...)"
  (let ((hook-var (make-symbol "hook-var"))
        (hook-val (make-symbol "hook-val")))
    ;; We assign to separate non-conflicting variables to only evaluate
    ;; our arguments once, in case they are not symbols/literals.
    `(let ((,hook-var ,hook)
           (,hook-val ,fn))
       (add-hook ,hook-var ,hook-val)
       (unwind-protect
           ,(macroexp-progn body)
         (remove-hook ,hook-var ,hook-val)))))

(defun ol-org-id--set-open-links-in-same-buffer-if-called-interactively (orig-fun &rest args)
  (interactive)
  (if (called-interactively-p 'any)
      (with-hook-added 'clone-indirect-buffer-hook
                       #'set-org-id-open-links-in-same-buffer
        ;; TODO: What about args? Does prefix get passed through?
        (call-interactively orig-fun))
    (apply orig-fun args)))

;; We advise existing functions instead of e.g. creating new commands so that:
;;
;; 1. No key bindings need to be updated
;; 2. We don't have to worry about setting the description of the keys to
;;    match the names of the functions
;; 3. We don't have to worry about forgetting to use a special function one
;;    time later on and not getting the expected behavior
;; These are all the functions related to making indirect buffers immediately
;; visible in the spacemacs buffer command selection, add more if I start to
;; use any more.
(let ((indirect-buffer-functions (list
                                   ;; TODO: doesn't work, since
                                   ;; `make-indirect-buffer' doesn't respect
                                   ;; `clone-indirect-buffer-hook'
                                   'make-indirect-buffer
                                   'clone-indirect-buffer
                                   'clone-indirect-buffer-other-frame
                                   'clone-indirect-buffer-other-window
                                   'clone-indirect-buffer-other-window-without-purpose)))
  (dolist (fn indirect-buffer-functions)
    (advice-add fn
                :around
                #'ol-org-id--set-open-links-in-same-buffer-if-called-interactively)))

;; This gives us a few nice properties compared to alternatives:
;; 1. We don't step on any registered org-open-at-point hooks
;; 2. Not dependent on the order of extension loading or hook registration
;; 3. Respects any wrapper around clone-indirect-buffer-other-frame
(defun org-open-at-point-indirect-buffer-other-frame ()
  "Open the current link. If it is an id-based link, then open it in a new
indirect buffer in a new frame. Similar to S-click in web browsers.
"
  (interactive)
  ;; First, emulate part of `org-open-at-point' to see if we're on an id-based
  ;; link.
  (when (ol-org-id--get-current-id-link)
    ;; By default links do not navigate in the same indirect buffer. We set
    ;; this to ensure that they are.
    (with-hook-added 'clone-indirect-buffer-hook
                     #'set-org-id-open-links-in-same-buffer
      (clone-indirect-buffer-other-frame nil)))
  ;; At this point, if applicable, we're in the context of the new buffer in
  ;; the new frame, so navigate to the link like usual.
  (org-open-at-point))

(provide 'ol-org-id)

;;; ol-org-id.el ends here
