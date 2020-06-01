;;; lang/arduino/autoload.el -*- lexical-binding: t; -*-

(defun +arduino-use-popup-system-a (orig-fn &rest args)
  "Use `display-buffer' instead of `switch-to-buffer'."
  (cl-letf (((symbol-function #'switch-to-buffer) #'display-buffer))
    (apply orig-fn args)))
