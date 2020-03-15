;;; editor/fmt/+onsave.el -*- lexical-binding: t; -*-

(defvar +fmt-on-save-enabled-modes
  '(:not fundamental-mode
         emacs-lisp-mode
         sql-mode
         tex-mode
         latex-mode)
  "A list of major modes in which to reformat the buffer upon saving.

If this list begins with `:not', then it negates the list.
If this is `t', it is enabled in all modes.
If `nil', it is disabled in all modes, as if the +onsave flag wasn't set.")

(defun +fmt--on-save-enabled-p (&optional mode)
  "Check if buffers of MODE should be formatted on save.

This is controlled by `+fmt-on-save-enabled-modes'."
  (if (booleanp +fmt-on-save-enabled-modes)
      +fmt-on-save-enabled-modes
    (xor (eq :not (car +fmt-on-save-enabled-modes))
         (memq (or mode major-mode) +fmt-on-save-enabled-modes))))

(defun +fmt--enable-fmt-on-save-mode-h ()
  "Format the buffer on save in certain major modes.
This is controlled by `+fmt-on-save-enabled-modes'."
  (when (+fmt--on-save-enabled-p)
    (+fmt-on-save-mode +1)))

(add-hook 'after-change-major-mode-hook '+fmt--enable-fmt-on-save-mode-h)
