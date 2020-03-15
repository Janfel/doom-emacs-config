;;; editor/fmt/autoload/settings.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'parent-mode)

;; Redefine to silence the linter.
(defvar +fmt-formatter-table)
(defvar +fmt-mode-formatter-table)

(defun +fmt--remove (formatter mode)
  (setf (gethash mode +fmt-mode-formatter-table)
        (delq formatter (gethash mode +fmt-mode-formatter-table))))

(defun +fmt--set-default (formatter mode)
  "Make FORMATTER the default formatter for MODE.
Both arguments are symbols."
  (setf (gethash mode +fmt-mode-formatter-table)
        (cons formatter
              (delq formatter (gethash mode +fmt-mode-formatter-table)))))

(defun +fmt--get-formatter-type--alist-valid-p (alist)
  (cl-flet ((num-valid-p (num) (or (null num) (and (numberp num) (> num 0))))
            (type-valid-p (type) (and type (not (eq type 'alist)))))
    (cl-flet ((cons-valid-p (elem) (and (num-valid-p (car elem))
                                        (type-valid-p
                                         (+fmt--get-formatter-type (cdr elem))))))
      (-all-p #'cons-valid-p alist))))

(defun +fmt--get-formatter-type (formatter)
  "Returns a symbol that describes the type of FORMATTER or nil."
  (cond ((keywordp formatter) 'reference)
        ((functionp formatter) 'function)
        ((stringp formatter) 'shell-command)
        ((and (consp formatter)
              (cond ((symbolp (car formatter)) 'sexpr)
                    ((stringp (car formatter)) 'shell-list)
                    ((and (consp (car formatter))
                          (+fmt--get-formatter-type--alist-valid-p formatter)
                          'alist)))))))


;;; Public Library

;;;###autoload
(defun +fmt-add (formatter &rest modes)
  (let (default)
    (cl-loop for mode in modes
             if (eq mode :default)
             do (setq default t)
             else if (or default (null (gethash mode +fmt-mode-formatter-table)))
             do (push formatter (gethash mode +fmt-mode-formatter-table))
             else
             do (push formatter (cdr (gethash mode +fmt-mode-formatter-table))))))

;;;###autoload
(defun +fmt-remove (formatter &rest modes)
  (if modes
      (mapc (apply-partially #'+fmt--remove formatter) modes)
    (remhash formatter +fmt-formatter-table)))

;;;###autoload
(cl-defun +fmt-define (name formatter &key mode when default)
  ""
  (unless (listp mode)
    (setq mode (list mode)))
  (puthash name
           (list formatter
                 :predicate (or when t)
                 :type (or (+fmt--get-formatter-type formatter)
                           (error "Invalid formatter expression: %s" formatter)))
           +fmt-formatter-table)
  (apply #'+fmt-add name (if default (cons :default mode) mode)))

;;;###autoload
(defun +fmt-set-default (formatter &rest modes)
  "Make FORMATTER the default formatter for MODES.
If MODES is nil, make FORMATTER the default fallback formatter.
All arguments are symbols."
  (mapc (apply-partially #'+fmt--set-default formatter) (or modes '(t))))

;;;###autoload
(defun +fmt-get-all ()
  (hash-table-keys +fmt-formatter-table))

;;;###autoload
(defun +fmt-applicable-p (formatter)
  (-some-> formatter
    (gethash +fmt-formatter-table)
    (cdr)
    (plist-get :predicate)
    (eval)))

;;;###autoload
(defun +fmt-get-applicable (&optional mode)
  (->> (or mode major-mode)
       (parent-mode-list)
       (cons t)
       (reverse)
       (--mapcat (gethash it +fmt-mode-formatter-table))
       (-filter #'+fmt-applicable-p)))

;;;###autoload
(defun +fmt-get (&optional mode)
  (car (+fmt-get-applicable mode)))

;;;###autoload
(defun +fmt-lookup (formatter)
  (gethash formatter +fmt-formatter-table))
