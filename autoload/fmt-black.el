;;; ~/.config/doom-emacs/autoload/fmt-black.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

;;;###autoload
(autoload 'black-format-buffer "autoload/fmt-black" nil t)
;;;###autoload
(autoload 'black-format-region "autoload/fmt-black" nil t)

(use-package! format-all :commands (format-all--buffer-extension-p))

;; Vanilla Black
(when! (not (eq SYSTEM 'Phantom))
  (formatter-define! black
    :program "black"
    :args
    `(,@(when (format-all--buffer-extension-p "pyi") (list "--pyi"))
      "-q" "-")))

;; Patched Black
(when! (eq SYSTEM 'Phantom)
  (formatter-define! black
    :program "black"
    :args
    `(,@(when indent-tabs-mode (list "-T"))
      ,@(when (format-all--buffer-extension-p "pyi") (list "--pyi"))
      "-q" "-")))
