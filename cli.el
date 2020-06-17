;;; ~/.config/doom-emacs/cli.el -*- lexical-binding: t; -*-

(defcli! tangle nil
  "Tangles your literate config files."
  (+literate-tangle-h 'force))
