;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (flycheck-disabled-checkers emacs-lisp-checkdoc)
  (eval remove-hook 'flycheck-mode-hook #'+emacs-lisp-reduce-flycheck-errors-in-emacs-config-h)))
