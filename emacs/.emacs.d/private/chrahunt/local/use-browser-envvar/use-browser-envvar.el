;;; use-browser-envvar.el --- Use the BROWSER environment variable

;; Author: Chris Hunt <chrahunt@gmail.com>
;; Version: 1.0

(setq
  browse-url-generic-program (getenv "BROWSER")
  browse-url-browser-function 'browse-url-generic
)

(provide 'use-browser-envvar)
