;;; autoload/cc-mode.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar *cc-use-pragma-once t
  "Prefer “pragma once” over “ifndef” header guards.")

;;;###autoload
(defun *cc-header-file-p (filename)
  "Return t if FILENAME is a C/C++ header file."
  (declare (pure t) (side-effect-free t))
  (let ((ext (file-name-extension filename)))
    (or (null ext) ;; Standard library header, such as “iostream”.
        (string-prefix-p "h" ext 'ignore-case))))

(defun *cc-make-cpp-token (name)
  "Generate a C preprocessor token based on NAME."
  (declare (pure t) (side-effect-free t))
  (upcase (replace-regexp-in-string "[^[:alnum:]]" "_" name 'fixedcase 'literal)))

;;;###autoload
(defun *cc-buffer-header-guard-token (&optional buffer)
  "Return a CPP token to be used as header guard for BUFFER."
  (declare (side-effect-free t))
  (*cc-make-cpp-token
   (if-let* ((bufname (buffer-file-name buffer)))
       (file-name-nondirectory bufname)
     (buffer-name buffer))))

;;;###autoload
(defun *cc-other-file-extension (filename)
  "Return the file extension the matching Header/Source file would have."
  (require 'find-file)
  (caadr (assoc filename cc-other-file-alist #'string-match-p)))
