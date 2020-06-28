;;; ~/.config/doom-emacs/cli.el -*- lexical-binding: t; -*-

(defcli! tangle (&optional file)
  "Tangles your literate config files."
  (require 'ob-tangle)
  (org-babel-tangle-file
   (or file
       +literate-config-file)))
