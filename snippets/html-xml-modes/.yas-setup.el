;;; snippets/<html-xml>/.yas-setup.el -*- lexical-binding: t; -*-


(defvar *yas-html-doctype-alist
  '(("HTML 5"                 . "html")
    ("HTML 4.01"              . "HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"")
    ("HTML 4.01 Transitional" . "HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\"")
    ("HTML 4.01 Frameset"     . "HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\"")))

(defvar *yas-xhtml-doctype-alist
  '(("XHTML 1.1"              . "HTML PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"")
    ("XHTML 1.0 Strict"       . "HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"")
    ("XHTML 1.0 Transitional" . "HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"")
    ("XHTML 1.0 Frameset"     . "HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\"")))

(defun *yas-choose-html-doctype ()
  (*yas-alist-choose (append *yas-html-doctype-alist
                             *yas-xhtml-doctype-alist)))

(defun *yas-choose-xml-doctype ()
  (*yas-alist-choose *yas-xhtml-doctype-alist))
