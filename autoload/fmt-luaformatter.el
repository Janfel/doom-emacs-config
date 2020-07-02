;;; ~/.config/doom-emacs/autoload/fmt-luaformatter.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar luaformatter-config-file nil
  "The configuration file for `luaformatter-format-region'.")

(defun luaformatter-compute-args ()
  "Compute arguments for `luaformatter-format-region'."
  (let ((indent      (if indent-tabs-mode 1 indent-level))
        (cont-indent (if indent-tabs-mode 1 indent-level)))
    (nconc
     (when (and luaformatter-config-file (file-readable-p luaformatter-config-file))
       (list "--config" luaformatter-config-file))
     (list
      (if indent-tabs-mode "--use-tab" "--no-use-tab")
      "--indent-width"               (number-to-string indent)
      "--continuation-indent-width"  (number-to-string cont-indent)
      "--tab-width"                  (number-to-string tab-width)))))

;;;###autoload (autoload 'luaformatter-format-buffer "autoload/fmt-luaformatter" nil t)
;;;###autoload (autoload 'luaformatter-format-region "autoload/fmt-luaformatter" nil t)
(formatter-define! luaformatter
  :program "lua-format"
  :args (luaformatter-compute-args))
