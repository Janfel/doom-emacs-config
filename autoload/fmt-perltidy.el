;;; ~/.config/doom-emacs/autoload/fmt-perltidy.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor fmt +define)

;;;###autoload (autoload 'perltidy-format-buffer "autoload/fmt-perltidy" nil t)
;;;###autoload (autoload 'perltidy-format-region "autoload/fmt-perltidy" nil t)
(formatter-define! perltidy
  :program "perltidy"
  :args
  `("--indent-columns" ,(number-to-string indent-level)
    "--default-tabsize" ,(number-to-string tab-width)
    ,(if indent-tabs-mode "--tabs" "--notabs")
    "--standard-output" "--standard-error-output"))
