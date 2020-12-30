;;; autoload/fmt-shfmt.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar shfmt-format-args '("-sr"))

(defun shfmt-compute-args ()
  "Compute arguments for `shfmt-format-region'."
  (nconc
   (and buffer-file-name (list "-filename" buffer-file-name))
   (when (and (derived-mode-p 'sh-mode) (bound-and-true-p sh-shell))
     (list "-ln" (symbol-name sh-shell)))
   (list "-i" (number-to-string (if indent-tabs-mode 0 standard-indent)))
   shfmt-format-args))

;;;###autoload (autoload 'shfmt-format-buffer "autoload/fmt-shfmt" nil t)
;;;###autoload (autoload 'shfmt-format-region "autoload/fmt-shfmt" nil t)
(+fmt-define shfmt
  :program "shfmt"
  :args (shfmt-compute-args))
