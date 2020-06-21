;;; ~/.config/doom-emacs/autoload/fmt-ptop.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

(defvar ptop-formatter-config
  (expand-file-name "pascal/ptop.cfg" XDG-CONFIG-HOME)
  "The configuration file for `ptop-format-buffer'.")

(defun ptop-compute-args ()
  "Compute arguments for `ptop-format-region'."
  (when IS-WINDOWS (error "PTOP can't be used on Windows"))
  (nconc
   (when (file-readable-p ptop-formatter-config)
     (list "-c" ptop-formatter-config))
   (list
    "-i" (number-to-string indent-level)
    "-l" (number-to-string fill-column)
    "/dev/stdin" "/dev/stdout")))

;;;###autoload (autoload 'ptop-format-buffer "autoload/fmt-ptop" nil t)
;;;###autoload (autoload 'ptop-format-region "autoload/fmt-ptop" nil t)
(formatter-define! ptop
  :program "ptop"
  :args (ptop-compute-args))
