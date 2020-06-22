;;; ~/.config/doom-emacs/autoload/fmt-black.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

;; Autoload `format-all--buffer-extension-p'.
(use-package! format-all :commands (format-all--buffer-extension-p))

(defun black-compute-args ()
  "Compute arguments for `black-format-region'."
  (nconc
   (when (and (sys-feature-p 'black/tabs) indent-tabs-mode) '("--use-tabs"))
   (when (format-all--buffer-extension-p "pyi") '("--pyi"))
   '("--quiet" "-")))

;;;###autoload (autoload 'black-format-buffer "autoload/fmt-black" nil t)
;;;###autoload (autoload 'black-format-region "autoload/fmt-black" nil t)
(formatter-define! black
  :program "black"
  :args (black-compute-args))
