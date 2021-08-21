;;; chrahunt/packages/cli.el -*- lexical-binding: t; -*-

(defcli! freeze-packages ()
  "Freeze packages."
  (doom-initialize-packages)
  (straight-freeze-versions t))

(defcli! thaw-packages ()
  "Thaw packages."
  (doom-initialize-core-packages)
  (straight-thaw-versions))
