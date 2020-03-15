;;; editor/fmt/config.el -*- lexical-binding: t; -*-


(defvar-local +fmt-formatter nil
  "Set this to override the default formatter for the current buffer.")

(defvar +fmt-preserve-indentation t
  "If non-nil, the leading indentation is preserved when formatting the whole
buffer. This is particularly useful for partials.

Indentation is always preserved when formatting regions.")

(defvar +fmt-formatter-table (make-hash-table :test 'eq)
  "Stuff.
Each key is a symbol that is the name of the formatter.
Each value is a cons whose car is the formatter and
whose cdr is a plist.")

(defvar +fmt-mode-formatter-table (make-hash-table :test 'eq)
  "Maps each major-modes to a formatter or list of formatters.")


;;(puthash t (list nil (lambda () (format-all-buffer) nil)) +fmt-formatter-table)

;; Formatter environment variables.

(defvar +fmt|prefix-arg nil
  "A prefix argument for the formatter.
This can be a positive integer or nil, if no prefix argument was given")


(define-minor-mode +fmt-on-save-mode
  "Toggle formatting on save.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :lighter "Fmt"
  (if +fmt-on-save-mode
      (add-hook  'before-save-hook '+fmt/buffer nil 'local)
    (remove-hook 'before-save-hook '+fmt/buffer     'local)))


(when (featurep! +onsave)
  (load! "+onsave"))
