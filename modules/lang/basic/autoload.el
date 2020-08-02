;;; lang/basic/autoload.el -*- lexical-binding: t; -*-

(defun +basic-denumber (beg end)
  "Remove line number from every line in the region BEG to END."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (basic-remove-line-number)
      (forward-line 1))))

;;;###autoload
(defun +basic/denumber ()
  "Remove line number from every line in current buffer/region."
  (interactive "*")
  (if (doom-region-active-p)
      (+basic-denumber (doom-region-beginning) (doom-region-end))
    (+basic-denumber (point-min) (point-max))))
