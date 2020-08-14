;;; autoload/subr-j.el -*- lexical-binding: t; -*-

(defun retcar (objects &optional repeat)
  (let ((objs (if repeat (nconc objects objects) objects)))
   (lambda (&rest _) (pop objs))))

;;;###autoload
(defmacro while-let (bindings &rest body)
  (let ((condition-sym (make-symbol "condition")))
    `(let ((,condition-sym t))
       (while ,condition-sym
         (if-let ,bindings
             ,(macroexp-progn body)
           (setq ,condition-sym nil))))))
