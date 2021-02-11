;;; autoload/fmt-luaformatter.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

;;; LuaFormatter - Code formatter for Lua
;;; https://github.com/Koihik/LuaFormatter

(defvar luaformatter-program "lua-format"
  "The executable for `luaformatter-format-region'.")

(defvar luaformatter-config-file nil
  "The configuration file for `luaformatter-format-region'.")

(defun luaformatter-compute-args ()
  "Compute arguments for `luaformatter-format-region'."
  (let ((indent      (if indent-tabs-mode 1 standard-indent))
        (cont-indent (if indent-tabs-mode 1 standard-indent)))
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
(+fmt-define luaformatter
  :program luaformatter-program
  :args (luaformatter-compute-args))
