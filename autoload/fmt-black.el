;;; autoload/fmt-black.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defun black-compute-args ()
  "Compute arguments for `black-format-region'."
  (nconc
   (when (and (sys-feature-p 'black/tabs) indent-tabs-mode) '("--use-tabs"))
   (when (string-suffix-p ".pyi" (or buffer-file-name "") t) '("--pyi"))
   (list "--line-length" (number-to-string fill-column) "--quiet" "-")))

;;;###autoload (autoload 'black-format-buffer "autoload/fmt-black" nil t)
;;;###autoload (autoload 'black-format-region "autoload/fmt-black" nil t)
(+fmt-define black
  :program "black"
  :args (black-compute-args))
