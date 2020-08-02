;;; autoload/jfl-electric-quotes.el -*- lexical-binding: t; -*-

(defvar electric-quote-chars-alist
  '(("english"    ?‘ ?’ ?“ ?”)
    ("german"     ?‚ ?‘ ?„ ?“)
    ("german-lit" ?› ?‹ ?» ?«)
    ("french"     ?‹ ?› ?« ?»)
    ("japanese"   ?『 ?』 ?「 ?」))
  "Map an identifier string to a value of `electric-quote-chars'.")

(defun electric-quote--chars-annotation (chars-name)
  (when-let ((chars (cdr (assoc chars-name electric-quote-chars-alist))))
    (cl-destructuring-bind (i1 i2 o1 o2) chars
      (format "%sText %cOuter %cInner%c Outer%c Text"
              (make-string (- 40 (length chars-name)) ? )
              o1 i1 i2 o2))))

(defun read-electric-quote-chars (prompt)
  "Read a name of an entry in `electric-quote-chars-alist' with PROMPT."
  (let ((completion-extra-properties
         '(:annotation-function electric-quote--chars-annotation))
        (default (car (rassoc electric-quote-chars electric-quote-chars-alist))))
    (completing-read prompt (mapcar #'car electric-quote-chars-alist)
                     nil 'require-match nil nil default)))

;;;###autoload
(defun set-electric-quote-chars (chars-name)
  "Set `electric-quote-chars' to the value named CHARS-NAME in `electric-quote-chars-alist'."
  (interactive
   (list (read-electric-quote-chars "Select quote chars: ")))
  (setq electric-quote-chars (cdr (assoc chars-name electric-quote-chars-alist))))
