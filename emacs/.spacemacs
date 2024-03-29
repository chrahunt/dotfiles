;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.emacs.d-private/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     ;; emacs-lisp
     ;; git
     ;; helm
     ;; lsp
     ;; markdown
     ;; multiple-cursors
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; treemacs
     ;; version-control
     pdf
     chrahunt
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
     ;; Provides yaml highlighting in src blocks.
     yaml-mode
     org-noter
     ;; Backup
     git-auto-commit-mode
     ;; dependencies
     org-edna
     ;; org-babel rust support
     rustic
     ;; org-babel jq support
     jq-mode
     ;; spaced repetition
     org-drill
     ;; project management
     ;ox-taskjuggler
   )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default spacemacs-27.1.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 10

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode nil

   ;; If non-nil activate `snoopy-mode' which shifts your number row
   ;; to match the set of signs given in `dotspacemacs-snoopy-keyrow'
   ;; in programming modes (insert-mode only). (default nil)
   dotspacemacs-use-snoopy-mode nil

   ;; Text of shifted values from your
   ;; keyboard's number row. (default '!@#$%^&*()')
   dotspacemacs-snoopy-keyrow "!@#$%^&*()"

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (require 'org-drill)
  ;; Do not save the list of packages, since packages and dependencies are
  ;; managed by spacemacs.
  (defun package--save-selected-packages (&rest opts) nil)
  ;; Pull from environment, since I use a different path on different machines.
  (setq org-directory (substitute-in-file-name "${EMACS_ORG_DIRECTORY}"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; Set default agenda files to notes file, since I use a single note everywhere.
  (setq org-agenda-files `(,org-default-notes-file))
  (setq evil-want-Y-yank-to-eol nil)
  (setq org-fontify-whole-heading-line t)
  (setq org-goto-auto-isearch nil)
  (setq org-hide-leading-stars t)
  (setq spacemacs-large-file-modes-list
       '(archive-mode
         tar-mode
         jka-compr
         git-commit-mode
         image-mode
         doc-view-mode
         doc-view-mode-maybe
         ebrowse-tree-mode
         pdf-view-mode
         tags-table-mode
         fundamental-mode
         org-mode))

  ;; Push any updates to auto-commited files automatically
  (setq-default gac-automatically-push-p t)
  ;; When inserting attachments and screenshots in org-mode, they should
  ;; also be added and auto-committed to the repository.
  (setq-default gac-add-additional-flag "-A .")
  ;; By default, git-auto-commit displays the result of executing git commit,
  ;; which usually causes the minibuffer to flash and be multi-line.
  ;; That's pretty intrusive, so I disable it.
  (setq-default gac-silent-message-p t)

  ;; Auto-save org-mode files after emacs is idle for 10s
  (defun save-org-mode-files ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'org-mode)
          (if (and (buffer-modified-p) (buffer-file-name))
            (save-buffer))))))

  (run-with-idle-timer 10 t 'save-org-mode-files)

  ;; No UTF-8 bullets
  (setq org-superstar-headline-bullets-list '("*"))

  ;; I do not indent content in org nodes because I see each node as an individual note.
  ;; Do not change indentation when promoting/demoting
  (setq org-adapt-indentation nil)

  (setq org-capture-templates '(
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
      (file+headline "" "tasks")
      "* TODO %^{Description}
:PROPERTIES:
:CREATED: %U
:END:

%?

%a
"
    )
    ("n" "Note" entry
      (file+headline "" "notes")
      "* TODO %^{Title} :note:
:PROPERTIES:
:CREATED: %U
:END:

%?

%a
"
    )
    ("i" "Interruption" entry
      (file+headline "" "notes")
      "* %^{Title} :unsorted:interruption:
:PROPERTIES:
:CREATED: %U
:END:

%?

%a
"
      :prepend t :clock-in t :clock-resume t
    )
  ))

  ;; Set default, to avoid being prompted "Symbolic link to Git-controlled
  ;; source file; follow link?" when editing .spacemacs. This disables VC-
  ;; related features, but I don't currently use those.
  (setq vc-follow-symlinks nil)

  ;; Custom key bindings setup. "o" is reserved for user-use
  (spacemacs/declare-prefix "o" "custom")
  (spacemacs/declare-prefix-for-mode 'org-mode "o" "custom")

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

  ;; By default, indirect buffer functions will inherit the state of the base
  ;; buffer, not the current (possibly indirect) buffer. In order to maintain
  ;; the context for making the new indirect buffer in the first place, we preserve
  ;; the properties of the indirect buffer.
  ;; Other code may make use of `clone-indirect-buffer-hook', so we run before that, so
  ;; that any other visibility changes will take place as-expected.
  ;; TODO

  ;; Required, otherwise on WSL the frame is created with squished features
  ;; that don't resolve until resizing the frame.
  (add-to-list 'default-frame-alist '(width . 138))
  (add-to-list 'default-frame-alist '(height . 120))

  ;; Enable inline evaluation of Python source blocks.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (dot . t) (emacs-lisp . t) (shell . t) (jq . t)))

  ;; Prevent src block auto-indentation
  ;; https://github.com/syl20bnr/spacemacs/issues/13255
  (setq org-src-preserve-indentation t)

  ;; Handles arbitrary date properties, defaulting to 1970-01-01.
  ;; Based on https://emacs.stackexchange.com/a/26369
  (defun my/org-agenda-cmp-date-property (prop)
    "Compare two `org-mode' agenda entries, `A' and `B', by some date property.

If a is before b, return -1. If a is after b, return 1. If they
are equal return t."
    (lexical-let ((prop prop))
      #'(
        lambda (a b)
        (let*
          (
            (a-pos (get-text-property 0 'org-marker a))
            (b-pos (get-text-property 0 'org-marker b))
            (a-date (or (org-entry-get a-pos prop) "[1970-01-01]"))
            (b-date (or (org-entry-get b-pos prop) "[1970-01-01]"))
            (cmp (compare-strings a-date nil nil b-date nil nil))
          )
          (if (eq cmp t) nil (signum cmp))
        )
      )
    )
  )

  (defun my/org-agenda-entry-not-in-tasks ()
    (
      let*
      ((outline-path (org-get-outline-path)))
      (if (not (and (eq (length outline-path) 1) (string= (car outline-path) "tasks"))) (point))
    )
  )

  (setq org-agenda-custom-commands
    '(
      ("o" . "Custom")
      ("oi" "Inbox"
        ;; +unsorted - items marked unsorted
        ;; -note - items not marked note
        ;; +TODO="TODO" - only in TODO state
        ;; -ARCHIVE_TIME={.} - where there is no ARCHIVE_TIME property
        ;;tags-todo "+unsorted-note+TODO=\"TODO\"-ARCHIVE_TIME={.}"
        tags-todo "TODO=\"TODO\"-ARCHIVE_TIME={.}"
        (
          ;; Ignore all entries except those under the top-level 'tasks' header
          (org-agenda-skip-function 'my/org-agenda-entry-not-in-tasks)
          ;; Show "Inbox:" instead of the search string as the first line of the agenda
          (org-agenda-overriding-header "Inbox:")
          ;; Sort by CREATED date
          (org-agenda-cmp-user-defined (my/org-agenda-cmp-date-property "CREATED"))
          ;; Sort descending
          (org-agenda-sorting-strategy '(user-defined-down))
          ;; Override the default prefix used for the item display on the agenda page.
          ;; Normally this will show the category, "notes:    ", as derived from the
          ;; filename, which is not useful here since I keep my inbox in 1 file.
          (org-agenda-prefix-format '((tags . "  ")))
          ;; Since we only care about top-level entries under "Tasks", tag inheritance
          ;; isn't relevant here. Disabling it to hopefully speed things up.
          (org-agenda-use-tag-inheritance nil)
        )
      )
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
          (org-agenda-use-tag-inheritance nil)
        )
      )
      ("on" "Related notes"
        (lambda (query-string)
          (interactive "P")
          (let*
            (
              ; this doesn't work because org-capture-fill-template works off of a buffer, not
              ; the template string directly...
              ;(query-string (org-capture-fill-template query-string))
              (org-agenda-custom-commands `(("x" "x" tags ,query-string)))
            )
            (org-agenda nil "x")
          )
        )
        "CONTEXT={%(org-id-get-create)}"
        (
         ;(org-agenda-overriding-header "Related notes:")
         ;; I don't use these.
         (org-use-tag-inheritance nil)
        )
      )
      ("op" "Projects"
        search ":project:"
        ;; Specify format
        (
          ;; Only show headlines and descriptions
          (org-overriding-columns-format "%25ITEM %description")
          (org-agenda-view-columns-initially t)
        )
      )
      ("oa" "Active project"
        tags "+project+TODO=\"INPROGRESS\""
        (
          (org-agenda-overriding-header "Active project:")
          ;; Prevent :project: from being inherited.
          (org-use-tag-inheritance nil)
        )
      )
      ("ot" "Project tasks"
        tags "TODO=\"NEXT\""
        (
          (org-agenda-overriding-header "Available tasks:")
        )
      )
      ("or" "Recurring tasks"
        ;; Any date with a "+" probably has a repeater
        search "+{SCHEDULED: <.+?\\+.+?>}"
        (
          (org-agenda-overriding-header "Recurring tasks:")
        )
      )
    )
  )

  (defun my/org-refile-to-clock ()
    (interactive)
    (org-refile 2))

  ;; TODO: Have a nicer way to refile to project
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((nil . (:maxlevel . 9))))

  ;; Raise an error instead of editing hidden text
  (setq org-catch-invisible-edits 'error)

  ;; Don't confirm when executing source blocks in my default notes file, since I do it often
  ;; and don't paste unsafe code blocks there.
  (setq org-confirm-babel-evaluate
    (lambda (language body)
      (not (string= (buffer-file-name) org-default-notes-file))))

  ;; Log notes into LOGBOOK drawer instead of the top of the note.
  (setq org-log-into-drawer t)
  ;; Just record the time because I can manually add a note if desired.
  (setq org-log-reschedule 'time)
  (setq org-log-redeadline 'time)

  ;; By default, evil mode search will jump to the next entry and highlight all entries
  ;; in the buffer. I don't want this behavior, because in large files when starting to
  ;; type a query it is very slow (multiple seconds). This might be related to having
  ;; collapsed sections making a lot more text "within the buffer" than would be normally.
  ;; I also think the movement of the buffer is distracting. Setting this to `nil' disables
  ;; the behavior.
  (setq evil-ex-search-interactive nil)

  ;; Org export backends can be set either by setting this variable or importing specific
  ;; ox- packages, not both.
  (setq org-export-backends '(ascii html latex taskjuggler md))

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

  (advice-add 'org-agenda-goto :around #'chrahunt/org-agenda-goto-advice)

  ;; When files are opened, delegate to xdg-open on Linux instead of run-mailcap.
  ;; This takes effect when clicking or pressing RET on file links.
  (add-to-list 'org-file-apps-gnu '(system . "xdg-open \"%s\""))

  ;; By default, emacs opens text files in emacs, but we want to open drawio files
  ;; in drawio (via the system opener, like xdg-open above).
  (add-to-list 'org-file-apps '("drawio" . system))

  ;; Utilities for creating drawio diagrams.
  (setq drawio-empty-diagram-contents "\
<mxfile\
 host=\"Electron\"\
 modified=\"2020-10-21T23:01:12.268Z\"\
 agent=\"draw.io/13.7.9 Electron/10.1.3\"\
 etag=\"SgnuPUlkHG53HNGEorAc\"\
 version=\"13.7.9\"\
 type=\"device\">\
<diagram id=\"jsyRF68S_4cu8rbGsDyh\"\
 name=\"Page-1\">\
ddGxDoMgEADQr2FHiIk7te3SyaEzkauQoGcQo+3XV4PUEtuFHI+Dg4Nw0c4XJ3t9QwWWMKpm\
wk+Esbxgy7jCM0BGKQ3SOKM226EyL4iJm45GwZAkekTrTZ9ijV0HtU9MOodTmvZAm1btZQMH\
qGppj3o3yuugRU53v4JpdKycxfe1MiZvMGipcPoiXhIuHKIPUTsLsGvzYl/CvvOf1c/FHHT+\
x4Yl2M9eJskP8fIN\
</diagram>\
</mxfile>\
")

  (defun chrahunt/create-attached-diagram (name)
    "Create an empty draw.io diagram as an attachment named {name}.drawio"
    (interactive "sCreate diagram named: ")
    (let* ((attach-dir (org-attach-dir 'get-create))
           (attachment-name (concat name ".drawio"))
           (absolute-attachment-path
            (concat (file-name-as-directory attach-dir) attachment-name)))
      (when (not (file-exists-p absolute-attachment-path))
        (with-temp-file absolute-attachment-path
          (insert drawio-empty-diagram-contents)))
      (org-attach-sync)
      attachment-name))

  (defun chrahunt/create-and-open-attached-diagram (name)
    "Create an empty draw.io diagram as an attachment named {name}.drawio, and open it."
    (interactive "sCreate diagram named: ")
    (let ((attachment-name (chrahunt/create-attached-diagram name)))
      (if (fboundp 'org-attach-open-link)
          ;; org-attach-open-link was refactored in org 9.4, drop this when 9.3 is no longer relevant
          (org-attach-open-link attachment-name)
        (org-attach-follow attachment-name nil))
      attachment-name))

  (defun chrahunt/create-and-open-and-link-to-attached-diagram (name description)
    "Create an empty draw.io diagram as an attachment named {name}.drawio, open it, and insert a
link to it in the current file."
    (interactive "sCreate diagram named: \nsLink description: ")
    (let ((attachment-name (chrahunt/create-and-open-attached-diagram name)))
      (org-insert-link nil (concat "attachment:" attachment-name) description)))

  ;; *i*nsert > *c*hart
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ic" #'chrahunt/create-and-open-and-link-to-attached-diagram)

  ;; Open "idea://" links in IntelliJ.
  (require 'ol)

  (defun org-idea-open (path _)
    (call-process "idea" nil 0 nil path))

  (org-link-set-parameters "idea"
                           :follow #'org-idea-open)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t (:background "#1a1a1a" :foreground "#1a1a1a"))))
 '(org-level-1 ((t (:inherit bold :background "gray10" :foreground "#90c695" :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit bold :background "#1a1a1a" :foreground "#9acd32" :weight bold :height 1.2))))
 '(org-level-3 ((t (:background "#1a1a1a" :foreground "#66cc99" :weight normal :height 1.1))))
 '(org-level-4 ((t (:background "#1a1a1a" :foreground "#03c9a9" :weight normal))))
 '(org-level-5 ((t (:background "#1a1a1a" :foreground "#90c695" :weight normal))))
 '(org-level-6 ((t (:background "#1a1a1a" :foreground "#9acd32" :weight normal))))
 '(org-level-7 ((t (:background "#1a1a1a" :foreground "#66cc99" :weight normal))))
 '(org-level-8 ((t (:background "#1a1a1a" :foreground "#03c9a9" :weight normal)))))
)
