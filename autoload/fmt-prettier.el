;;; ~/.config/doom-emacs/autoload/fmt-prettier.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

;;;###autoload (autoload 'prettier-format-buffer "autoload/fmt-prettier" nil t)
;;;###autoload (autoload 'prettier-format-region "autoload/fmt-prettier" nil t)
(formatter-define! prettier
    :program "prettier"
    :args
    `("--tab-width" ,(number-to-string indent-level)
      ,@(when indent-tabs-mode '("--use-tabs"))))
