;;; chrahunt/packages/cli.el -*- lexical-binding: t; -*-

(defcli! freeze-packages ()
  "Freeze packages."
  (doom-initialize-packages)
  (straight-freeze-versions t))

(defcli! thaw-packages ()
  "Thaw packages."
  ;; `doom--ensure-straight' tries to change the straight.el branch name too
  ;; early, which fails. Running `straight-thaw-versions' after that fails, so
  ;; we unconditionally ensure the branch name is correct here.
  (let* ((repo-dir (doom-path straight-base-dir "straight/repos/straight.el"))
         (default-directory repo-dir))
    (doom-call-process "git" "branch" "-m" straight-repository-branch))
  ;; Initializes all recipes and reads `package!' declarations.
  (doom-initialize-packages)
  (straight-thaw-versions))
