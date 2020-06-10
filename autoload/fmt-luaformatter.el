;;; ~/.config/doom-emacs/autoload/fmt-luaformatter.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar luaformatter-config-file
  (expand-file-name "luaformatter/config.yaml" XDG-CONFIG-HOME)
  "The configuration file for `luaformatter-format-region'.")

(defun luaformatter-compute-args ()
  "Compute arguments for `luaformatter-format-region'."
  (let ((indent      (if indent-tabs-mode 1 indent-level))
        (cont-indent (if indent-tabs-mode 1 indent-level)))
    (list
     (if indent-tabs-mode "--use-tab" "--no-use-tab")
     (format "--indent-width=%d" indent)
     (format "--continuation-indent-width=%d" cont-indent)
     (format "--tab-width=%d" tab-width)
     (when (file-readable-p luaformatter-config-file)
       (format "--config=%s" luaformatter-config-file)))))

;;;###autoload (autoload 'luaformatter-format-buffer "autoload/fmt-luaformatter" nil t)
;;;###autoload (autoload 'luaformatter-format-region "autoload/fmt-luaformatter" nil t)
(formatter-define! luaformatter
  :program "lua-format"
  :args (luaformatter-compute-args))
