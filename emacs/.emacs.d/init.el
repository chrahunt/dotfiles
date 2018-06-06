(require 'package)
(require 'cl)
(defvar elpa-packages '(
  use-package
  ))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p elpa-packages)))
    (when pkgs
      (message "%s" "Emacs refresh packages database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p elpa-packages)
        (package-install p)))))

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(cfg:install-packages)

(require 'use-package)

(use-package evil
  :ensure t
  :demand
  :config
  (evil-mode 1)
  :bind (
    :map evil-normal-state-map
    ("C-u" . evil-scroll-up)
    ("C-h" . evil-window-left)
    ("C-j" . evil-window-down)
    ("C-k" . evil-window-up)
    ("C-l" . evil-window-right)))

(use-package neotree
  :ensure t)

(use-package org
  :ensure t)

(use-package evil-surround
  :ensure t)

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 0)
(tool-bar-mode 0)
