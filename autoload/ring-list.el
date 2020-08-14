;;; autoload/ring-list.el -*- lexical-binding: t; -*-

(defun rlist (&rest args)
  (declare (pure t) (side-effect-free t))
  (nconc args args))

(defun make-rlist (length init)
  (declare (pure t) (side-effect-free t))
  (let ((l (make-list length init)))
    (nconc l l)))

(defun rlist-to-list (rlist)
  (let ((p rlist)
        (newlist nil))
    (while (not (eq (cdr p) rlist))
      (push (car p) newlist))
    (nreverse newlist)))
