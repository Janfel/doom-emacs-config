;;; autoload/conceal.el  -*- lexical-binding: t; -*-

(defvar conceal-string-function #'default-conceal-string)

(defun default-conceal-string (text)
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string "[[:graph:]]" "▬" text 'fixedcase 'literal))

;;;###autoload
(defun conceal-line ()
  (interactive)
  (conceal-clear-line)
  (let ((ov (make-overlay (line-beginning-position)
                          (line-end-position)
                          (current-buffer)
                          'front-advance)))
    (overlay-put ov 'type 'conceal)
    (overlay-put ov 'display
                 (funcall conceal-string-function
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))))

(defun conceal-clear-line ()
  (interactive)
  (remove-overlays
   (line-beginning-position)
   (line-end-position)
   'type 'conceal))

;;;###autoload
(defun conceal-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (conceal-line)
      (forward-line))))

(defun conceal-clear-region (beg end)
  (interactive "r")
  (save-excursion
    (setq beg (progn (goto-char beg) (line-beginning-position)))
    (setq end (progn (goto-char end) (line-end-position))))
  (remove-overlays beg end 'type 'conceal))

;;;###autoload
(defun conceal-buffer (&optional buffer)
  (interactive)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (conceal-mode +1)))

(defun conceal-clear-buffer (&optional buffer)
  (interactive)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (conceal-mode -1)))

;;;###autoload
(define-minor-mode conceal-mode
  "Toggle concealement of buffer contents.

If called interactively, enable conceal mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
toggle; disable the mode otherwise.

When conceal mode in enabled, the buffers content will be obscured
by an overlay computed by `conceal-string-function'."
  :lighter nil
  (save-restriction
    (widen)
    (if conceal-mode
        (conceal-region (point-min) (point-max))
      (remove-overlays (point-min) (point-max) 'type 'conceal))))


(defvar conceal-buffer-predicate-functions
  '(conceal-gpg-buffer-p))

(defun conceal-gpg-buffer-p (buffer)
  (string-match-p "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'" (buffer-name buffer)))

(defun conceal-buffer-p (&optional buffer)
  (run-hook-with-args-until-success
   'conceal-buffer-predicate-functions
   (or buffer (current-buffer))))

(defun conceal-mode--maybe-enable ()
  (when (conceal-buffer-p)
    (conceal-mode +1)))

;;;###autoload
(define-globalized-minor-mode auto-conceal-mode
  conceal-mode conceal-mode--maybe-enable)
