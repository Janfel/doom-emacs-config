;;; lang/basic/autoload/evil-matchit-fbasic.el -*- lexical-binding: t; -*-

;; WIP

;; (defvar evilmi-fbasic-match-tags
;;   (append '(("IF" ("THEN" "ELSEIF" "ELSE IF" "ELSE") ("ENDIF" "END IF"))
;;             ("MACRO" () "ENDMACRO")
;;             ("SELECT CASE" ("CASE" "CASE ELSE") "END SELECT")
;;             (("DO UNTIL" "DO WHILE") () "LOOP")
;;             ("DO" () ("LOOP UNTIL" "LOOP WHILE")))
;;           (mapcar (lambda (kw) (list kw nil (concat "END " kw)))
;;                   fbasic-block-keywords)))
