;;; ~/.config/doom-emacs/autoload/fmt-shfmt.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar shfmt-config-file nil
  "The configuration file for `shfmt-format-region'.")

(defun shfmt-compute-args ()
  "Compute arguments for `shfmt-format-region'."
  (nconc
   (when (and (derived-mode-p 'sh-mode)
              (bound-and-true-p 'sh-shell))
     (list "-ln" (symbol-name sh-shell)))
   (list "-i" (number-to-string (if indent-tabs-mode 0 indent-level)))))

;;;###autoload (autoload 'shfmt-format-buffer "autoload/fmt-shfmt" nil t)
;;;###autoload (autoload 'shfmt-format-region "autoload/fmt-shfmt" nil t)
(formatter-define! shfmt
  :program "shfmt"
  :args (shfmt-compute-args))