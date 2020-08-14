;;; autoload/yasnippet.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor snippets)

;;;###autoload
(defun *yas-alist-choose (pairs)
  "Prompt for a pair in PAIRS with (car pair) returning (cdr pair).
PAIRS is an alist where the keys are prompt strings that identify the values."
  (unless (or yas-moving-away-p yas-modified-p)
    (cdr (cl-some (lambda (fn) (funcall fn "Choose: " pairs #'car))
                  yas-prompt-functions))))
