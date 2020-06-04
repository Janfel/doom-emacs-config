;;; ~/.config/doom-emacs/autoload/fmt-prettier.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

(defvar-local prettier-format-parser nil
  "The parser that Prettier should use.")

(defvar prettier-format--parser-alist
  '((js-mode         . "babel")
    (js2-mode        . "babel")
    (js3-mode        . "babel")
    (typescript-mode . "typescript")
    (css-mode        . "css")
    (scss-mode       . "scss")
    (less-css-mode   . "less")
    (json-mode       . "json")
    (graphql-mode    . "graphql")
    (markdown-mode   . "markdown")
    (html-mode       . "html")
    (yaml-mode       . "yaml")))

;;;###autoload (autoload 'prettier-format-buffer "autoload/fmt-prettier" nil t)
;;;###autoload (autoload 'prettier-format-region "autoload/fmt-prettier" nil t)
(formatter-define! prettier
    :program "prettier"
    :args
    `("--print-width" ,(number-to-string fill-column)
      "--tab-width" ,(number-to-string indent-level)
      ;; TODO: Use --cursor-offset --range-start --range-end.
      ,@(-some->> buffer-file-name (list "--stdin-filepath"))
      ,@(when indent-tabs-mode '("--use-tabs"))
      ,@(-some->> (or prettier-format-parser
                      (assq major-mode prettier-format--parser-alist))
          (list "--parser"))))
