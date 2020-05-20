;;; ~/.config/doom-emacs/autoload/fmt-luaformatter.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt)

(defvar luaformatter-config-file
  (expand-file-name "luaformatter/config.yaml" XDG-CONFIG-HOME)
  "The configuration file for `luaformatter-format-region'.")

;;;###autoload (autoload 'luaformatter-format-buffer "autoload/fmt-luaformatter" nil t)
;;;###autoload (autoload 'luaformatter-format-region "autoload/fmt-luaformatter" nil t)
(formatter-define! luaformatter
  ;; This program doesn’t accept configuration switches.
  ;; See https://github.com/Koihik/LuaFormatter/issues/110
  ;; for progress on this issue.
  :program "lua-format"
  :args
  (when (file-readable-p luaformatter-config-file)
    (list "--config" luaformatter-config-file)))
