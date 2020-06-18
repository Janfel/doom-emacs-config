;;; ~/.config/doom-emacs/autoload/fmt-prettier.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

(defvar-local prettier-format-parser nil
  "The parser that Prettier should use.")

(defvar prettier-format--parser-alist
  '(;; HTML
    (html-mode       . "html")
    (markdown-mode   . "markdown")
    ;; CSS
    (css-mode        . "css")
    (scss-mode       . "scss")
    (less-css-mode   . "less")
    ;; JavaScript
    (js-mode         . "babel")
    (js2-mode        . "babel")
    (js3-mode        . "babel")
    ;; TypeScript
    (typescript-mode . "typescript")
    ;; PHP
    (php-mode        . "php")
    ;; JSON
    (json-mode       . "json")
    (yaml-mode       . "yaml")
    ;; GraphQL
    (graphql-mode    . "graphql")))

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
