;;; lang/meson/autoload.el -*- lexical-binding: t; -*-

(defun +meson--find-reference-manual ()
  (let ((default-directory meson-markdown-docs-dir))
    (expand-file-name
     (seq-find #'file-exists-p
               '("Reference-manual.md"
                 "Reference-manual.md.gz")))))

(defun +meson--lookup-regexp (identifier)
  (rx bol (or (+ ?#) ?-) ?  (? ?`)
      (literal identifier)
      (or ?\( ?` eol)))

(defun +meson--find-in-reference-manual (identifier)
  (and (re-search-forward (+meson--lookup-regexp identifier) nil t)
       (line-beginning-position)))

;;;###autoload
(defun +meson-lookup-doc (identifier)
  (when-let* ((refman (+meson--find-reference-manual))
              (buf (find-file-noselect refman))
              (pos (with-current-buffer buf
                     (+meson--find-in-reference-manual identifier))))
    (pop-to-buffer buf)
    (markdown-view-mode)
    (read-only-mode)
    (local-set-key (kbd "q") 'bury-buffer)
    (local-set-key (kbd "C-g") 'bury-buffer)
    (goto-char pos)
    (recenter 0)
    (current-buffer)))
