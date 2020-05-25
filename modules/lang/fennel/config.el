;;; lang/fennel/config.el -*- lexical-binding: t; -*-

(use-package! fennel-mode
  :mode "\\.fnl\\'"
  :interpreter "fennel"
  :config
  ;; HACK: Disable `lisp-mode-hook' when starting `fennel-mode'.
  (add-hook! 'prog-mode-hook
    (defun +fennel--disable-lisp-mode-hook-in-fennel-mode-h ()
      (when (derived-mode-p 'fennel-mode)
        (setq-local lisp-mode-hook nil))))

  (add-hook 'fennel-mode-hook #'rainbow-delimiters-mode)
  ;; Improve indentation of variable definitions.
  (put 'var    'fennel-indent-function 'defun)
  (put 'local  'fennel-indent-function 'defun)
  (put 'global 'fennel-indent-function 'defun)

  (when! (featurep! :tools eval)
   (defadvice! +fennel--return-repl-buffer-from-fennel-repl-a (&rest _)
     "Make `fennel-repl' return the buffer it switches to.
This enables `fennel-repl' to be used with `set-repl-handler!'."
     :filter-return #'fennel-repl
     (get-buffer inferior-lisp-buffer))
   (set-repl-handler! 'fennel-mode #'fennel-repl))

  (when! (featurep! :tools lookup)
    (defadvice! +fennel--simplify-fennel-find-definition-a ()
      "Remove code that is made obsolete by `set-lookup-handlers!'."
      :override #'fennel-find-definition
      (fennel-find-definition-go (fennel-find-definition-for (symbol-at-point))))

    (set-lookup-handlers! 'fennel-mode
      :definition #'fennel-find-definition)
    ;; Made obsolete by `set-lookup-handlers!'.
    (map! :map fennel-mode-map "M-." nil "M-," nil)))
