;;; use-browser-envvar.el --- Use the BROWSER environment variable

;; Author: Chris Hunt <chrahunt@gmail.com>
;; Version: 1.0

(let ((browser (getenv "BROWSER")))
  (when browser
    (setq browse-url-generic-program browser
          browse-url-browser-function 'browse-url-generic)))

(provide 'use-browser-envvar)
