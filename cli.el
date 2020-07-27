;;; ~/.config/doom-emacs/cli.el -*- lexical-binding: t; -*-

(when (featurep! :config literate)
  (remove-hook 'doom-cli-pre-hook #'+literate-tangle-h)
  (add-hook! 'doom-cli-pre-hook
    (defun *literate-tangle-cached-h ()
      (when (file-newer-than-file-p +literate-config-file +literate-config-cache-file)
        (+literate-tangle-h)))))
