;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; Pull from environment, since I use a different path on different machines.
(setq org-directory (substitute-in-file-name "${EMACS_ORG_DIRECTORY}"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files `(,org-default-notes-file))

;; Add org-id to default-loaded org modules so that id-based links and
;; variables like `org-id-link-to-org-use-id' are respected.
;; https://github.com/hlissner/doom-emacs/issues/2643#issuecomment-593214284
(add-to-list 'org-modules 'org-id)
;; Map "," to local leader to align with spacemacs.
;; What's great is that this still seems to work with evil-snipe-repeat-reverse
;; when a search has been initiated! Just press Esc to pop off the transient map.
(setq doom-localleader-key ",")

(setq chrahunt/org-capture-templates
      '(
        ;; Files default to org-default-notes-file
        ;; Entries have
        ;; - binding
        ;; - name
        ;; - type
        ;; - target
        ;;   - type
        ;;   - file (default is org-default-notes-file)
        ;;   - header
        ;; - format
        ("t" "To Do" entry
         (file+olp "" "tasks")
         "* TODO %^{Description}
:PROPERTIES:
:CREATED: %U
:END:

%?

%a
")
        ("n" "Note" entry
         (file+olp "" "notes")
         "* TODO %^{Title} :note:
:PROPERTIES:
:CREATED: %U
:END:

%?

%a
")
        ("i" "Interruption" entry
         (file+olp "" "notes")
         "* %^{Title} :unsorted:interruption:
:PROPERTIES:
:CREATED: %U
:END:

%?

%a
" :prepend t :clock-in t :clock-resume t)))

(setq chrahunt/org-agenda-custom-commands
      '(
        ("o" . "Custom")
        ("os" "Scheduled"
         agenda ""
         (
          ;; Show scheduled items or appointments. Deadlines are handled
          ;; as part of higher-level planning.
          (org-agenda-entry-types '(:scheduled :timestamp))
          ;; Show today and tomorrow
          (org-agenda-span 2)
          ;; Start week on today
          (org-agenda-start-on-weekday nil)
          ;; No really, show the current day
          (org-agenda-start-day nil)
          ;; Override the default prefix used for the item display on the agenda page.
          ;; Normally this will show the category, "notes:    ", as derived from the
          ;; filename, which is not useful here since all my tasks are in 1 file.
          ;; The description of the format string is in `org-agenda-prefix-format',
          ;; here we just use the default without `%c'.
          (org-agenda-prefix-format '((agenda . " %i %?-12t% s")))
          ;; By default, tags are inherited. I don't use tag inheritance in my task
          ;; organization, so disabling them removes some clutter from the agenda
          ;; view itself.
          (org-agenda-use-tag-inheritance nil)))))

(defun chrahunt/save-all-org-buffers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-mode)
        (if (and (buffer-modified-p) (buffer-file-name))
            (save-buffer))))))

;; Prevent message when entering in org-babel python blocks
;; "Can't guess python-indent-offset"
(setq-hook! python-mode python-indent-offset 4)

;; Prevent initial tab
(setq-hook! text-mode electric-indent-inhibit t)

(after! org
  (setq
   ;; This is slow for me and takes up too much horizontal space in my docs.
   org-startup-indented nil
   ;; I like seeing all the stars in headlines.
   org-hide-leading-stars nil
   ;; Note: the actual capture template definition is kept out of line so it
   ;; is easier to read.
   org-capture-templates chrahunt/org-capture-templates
   org-agenda-custom-commands chrahunt/org-agenda-custom-commands
   ;; I use notes (`org-add-note') to keep track of task state changes
   ;; and keep asides having to do with execution of the task or updates
   ;; on blockers.
   ;; By default, notes are added to a list at the top of the current section -
   ;; put them into the LOGBOOK drawer instead.
   org-log-into-drawer t
   ;; Just record the time. Alternatively it could record a note, but I can
   ;; do that manually if needed.
   org-log-reschedule 'time
   org-log-redeadline 'time
   ;; When `org-store-link` is executed, create an :ID: property for the
   ;; corresponding node and store that, instead of using the headline.
   ;; This makes it easier to move nodes around without breaking links.
   ;; Since I only use my own org files and this flexibility is important to
   ;; me, I don't really care if extra :ID: properties get created when doing
   ;; an org-capture.
   org-id-link-to-org-use-id t
   ;; When an agenda view is created, split the current frame to accommodate it.
   org-agenda-window-setup 'reorganize-frame)

  ;; Save org files after 10 seconds idle.
  (run-with-idle-timer 10 t #'chrahunt/save-all-org-buffers)

  ;; Permit storing links to info files and navigating via info links in an org
  ;; buffer (`org-store-link'/`org-open-at-point')
  (add-to-list 'org-modules 'ol-info)

  ;; By default, doom configures `visual-line-mode' with `text-mode-hook'.
  ;; I don't like it generally, so disable it by default - it can be re-enabled
  ;; if needed with (SPC t w).
  (add-hook! org-mode
             (visual-line-mode -1))

  ;; Workaround for error seen after executing babel src block:
  ;;     Error (org-babel-after-execute-hook): Error running hook "org-redisplay-inline-images" because: (error Invalid base64 data)
  ;; from https://github.com/hlissner/doom-emacs/issues/3185
  (defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
    :override #'+org-inline-image-data-fn
    "Interpret LINK as base64-encoded image data. Ignore all errors."
    (ignore-errors
      (base64-decode-string link)))

  (map! :map org-mode-map
        :localleader
        ;; By default, this is mapped to `org-toggle-item'. spacemacs uses it
        ;; for "insert", and that's what I have muscle memory for, so overwrite
        ;; it.
        (:prefix ("i" . "insert")
         "b" #'org-insert-structure-template
         "n" #'org-add-note))

  (custom-set-faces!
    ;; Make the background of every org level darker, to act as separators between individual
    ;; sections.
    '(org-level-1 :inherit bold :background "gray10" :foreground "#90c695" :weight bold :height 1.3)
    '(org-level-2 :inherit bold :background "#1a1a1a" :foreground "#9acd32" :weight bold :height 1.2)
    '(org-level-3 :background "#1a1a1a" :foreground "#66cc99" :weight normal :height 1.1)
    '(org-level-4 :background "#1a1a1a" :foreground "#03c9a9" :weight normal)
    '(org-level-5 :background "#1a1a1a" :foreground "#90c695" :weight normal)
    '(org-level-6 :background "#1a1a1a" :foreground "#9acd32" :weight normal)
    '(org-level-7 :background "#1a1a1a" :foreground "#66cc99" :weight normal)
    '(org-level-8 :background "#1a1a1a" :foreground "#03c9a9" :weight normal)))

(use-package! org-in-buffer-capture-templates
  :after org
  :config
  ;; Align with capture menu in spacemacs.
  (map! :leader
        (:prefix ("C" . "capture")
         ("c" #'org-capture)
         ("b" #'org-capture-from-buffer))))

(use-package! org-download-z
  :after org
  :config
  ;; TODO: Move this keybinding to be alongside the other attachment-related
  ;; bindings in doom.
  (map! :map org-mode-map
        :localleader
        (:prefix ("i" . "insert")
         (:prefix ("D" . "download")
          ("p" #'org-download-z-paste)))))

(use-package! cibof
  :config
  (map! :leader
        (:prefix ("b" . "buffer")
         ("f" #'clone-indirect-buffer-other-frame))))

;; Helper function for working with frames. It doesn't seem like
;; frame operations get much attention in the default key bindings,
;; so I set those up here, too.
(use-package! frame-prefix
  :config
  (frame-prefix/register-frame-title-format)
  (map! :leader
        (:prefix ("F" . "frame")
         ("p" #'frame-prefix/set)
         ("r" #'frame-prefix/reset))))

;; These next 2 functions make `org-agenda-goto' use the buffer in the adjacent window (if its
;; base buffer contains the selected item) instead of changing the adjacent window to point to
;; the base buffer.
(defun chrahunt/org-agenda-goto-stbow-advice (args)
  (let ((buffer (car args))
        other-buffer)
    (save-window-excursion
      (other-window 1)
      (setq other-buffer (current-buffer)))
    (if (eq buffer (buffer-base-buffer other-buffer))
        (list other-buffer)
      (list buffer))))

(defun chrahunt/org-agenda-goto-advice (oldfun &rest args)
  (with-advice-added 'switch-to-buffer-other-window
                     :filter-args
                     #'chrahunt/org-agenda-goto-stbow-advice
                     (apply oldfun args)))

;; Provide helpful org-id-based link navigation in indirect buffers
(use-package! ol-org-id
  :after org
  :config
  (advice-add 'org-agenda-goto :around #'chrahunt/org-agenda-goto-advice)
  ;; Map S-RET to open id links in an indirect buffer in a new frame, similar
  ;; to S-click in web browsers.
  (map! :map org-mode-map
        ;; Terminal
        "S-RET" #'org-open-at-point-indirect-buffer-other-frame
        ;; GUI
        "S-<return>" #'org-open-at-point-indirect-buffer-other-frame))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; I use emacs mostly for org, and line numbers aren't as useful for me there.
(setq display-line-numbers-type nil)

;;
;; Makes fcitx more aware of editors modes, like:
;; - disables fcitx when in Evil normal mode
;; - disables fcitx in minibuffer
;; - others, see README in repo for details: https://github.com/cute-jumper/fcitx.el
(use-package! fcitx
  :config
  (fcitx-aggressive-setup))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
