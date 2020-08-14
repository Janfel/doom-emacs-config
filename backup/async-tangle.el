;;; autoload/async-tangle.el -*- lexical-binding: t; -*-

(require 'async)

;;;###autoload
(defun *async-tangle-file (file &optional target-file lang-re finish-func)
  "Like `org-babel-tangle-file' but in an asynchronous process.
FILE, TARGET-FILE and LANG-RE are passed to `org-babel-tangle-file'.
The return value is passed to FINISH-FUNC, which takes at least one argument.
This function returns a future that can be awaited with `async-wait'."
  (let ((file (expand-file-name file))
        (target-file (when target-file (expand-file-name target-file))))
    (async-start
     (lambda ()
       (require 'ob-tangle)
       (require 'ox)
       (let (write-file-functions
             before-save-hook
             after-save-hook
             org-mode-hook)
         (set-buffer (find-file-noselect file))
         (org-export-expand-include-keyword)
         (mapcar #'expand-file-name
                 (org-babel-tangle nil target-file lang-re))))
     (or finish-func #'ignore))))

;;;###autoload
(defun *async-tangle-h ()
  "Tangle the file the current buffer is visiting with `*async-tangle-file'."
  (when-let* (file (buffer-file-name))
    (let ((fname (file-name-nondirectory file))
          (start (current-time)))
      (message "Tangling %s..." fname)
      (*async-tangle-file
       file nil nil
       (lambda (target-files)
         (cl-dolist (buf (mapcar #'find-buffer-visiting target-files))
           (when buf (with-current-buffer buf (revert-buffer 'ignore-auto))))
         (message "Tangling %s into %s finished in %.02f seconds"
                  fname (mapcar #'file-name-nondirectory target-files)
                  (float-time (time-since start))))))))

;;;###autoload
(defun *literate-tangle-async-h ()
  "Tangle all “.org” files in `doom-private-dir' with `*async-tangle-file'."
  ;; TODO: Implement real recompilation system.
  (let* ((start (current-time))
         (cache-file +literate-config-cache-file)
         (files (seq-filter
                 (lambda (f) (file-newer-than-file-p f cache-file))
                 (directory-files doom-private-dir 'full (rx ".org" eos))))
         (counter (length files)))
    (cl-dolist (file files)
      (*async-tangle-file
       file nil nil
       (lambda (_)
         (when (zerop (cl-decf counter))
           (with-temp-file cache-file)
           (message "Tangling config files finished in %.02f seconds"
                    (float-time (time-since start)))))))))
