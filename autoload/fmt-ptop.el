;;; ~/.config/doom-emacs/autoload/fmt-ptop.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

(defvar ptop-formatter-config
  (expand-file-name "pascal/ptop.cfg" XDG-CONFIG-HOME)
  "The configuration file for `ptop-format-region'.")

;;;###autoload (autoload 'ptop-format-buffer "autoload/fmt-ptop" nil t)
;;;###autoload (autoload 'ptop-format-region "autoload/fmt-ptop" nil t)
(formatter-define! ptop
  :program "ptop"
  :args
  `(,@(-some->> ptop-formatter-config (list "-c"))
    "-i" ,(number-to-string indent-level)
    "/dev/stdin" "/dev/stdout"))
