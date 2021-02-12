;;; config/literate/autoload.el -*- lexical-binding: t; -*-

(defvar +literate-config-files
  (list (concat doom-private-dir "config.org"))
  "A list of `org-mode' files to be tangled automatically.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file to serve as mtime cache for tangling `+literate-config-files'.")

(defvar org-mode-hook)
(defvar org-inhibit-startup)
(defvar org-babel-pre-tangle-hook)
(defvar org-babel-tangle-body-hook)
(defvar org-babel-post-tangle-hook)


(defun +literate--tangle-lambda (file)
  (lambda ()
    (let ((write-file-functions nil)
          (before-save-hook nil)
          (after-save-hook nil)
          ;; Speed up org-mode startup.
          (org-mode-hook nil)
          (org-inhibit-startup t)
          ;; Do not overwrite the original file with `save-buffer'.
          (org-babel-pre-tangle-hook  nil)
          (org-babel-tangle-body-hook nil)
          (org-babel-post-tangle-hook nil))
      (require 'org)
      (require 'ob-tangle)
      ;; (require 'ox)
      ;; (org-export-expand-include-keyword)
      (with-temp-buffer
        (insert-file-contents file 'visit)
        (org-mode)
        (cons file (org-babel-tangle))))))

;;;###autoload
(defun +literate-tangle-file (file &optional callback sync)
  (interactive
   (list (or buffer-file-name (user-error "Not visiting a file"))
         (let ((start (current-time)))
           (lambda (x) (message "Tangling %s finished in %.02f seconds"
                                (file-relative-name (car x))
                                (float-time (time-since start)))))))
  (unless (file-readable-p file) (user-error "File not accessible"))
  (let ((func (+literate--tangle-lambda file))
        (callback (or callback #'ignore)))
    (if (and (not sync) (require 'async))
        (async-start func callback)
      (when doom-interactive-p
        (error "It is currently not safe to tangle synchronously"))
      (funcall callback (funcall func)))))

;;;###autoload
(defun +literate-tangle-file-h () (call-interactively #'+literate-tangle-file))

;;;###autoload
(defun +literate-tangle-config ()
  "Tangle `+literate-config-files' asynchronously.
Use this function when in interactive mode."
  (interactive)
  (let* ((start (current-time))
         (cache-file +literate-config-cache-file)
         (files (cl-loop for file in +literate-config-files
                         if (file-newer-than-file-p file cache-file)
                         collect file))
         (counter (length files))
         (callback (lambda (_)
                     (when (zerop (cl-decf counter))
                       (with-temp-file cache-file)
                       (message "Tangling %s finished in %.02f seconds"
                                (cl-loop for f in files collect
                                         (file-relative-name f doom-private-dir))
                                (float-time (time-since start)))))))
    (cl-dolist (file files) (+literate-tangle-file file callback))))

(defun +literate-tangle-config-sync-h ()
  (unless (getenv "__NOTANGLE")
    (print! (start "Compiling your literate config..."))
    (let* ((start (current-time))
           (cache-file +literate-config-cache-file)
           (files (cl-loop for file in +literate-config-files
                           if (file-newer-than-file-p file cache-file)
                           collect file))
           (count (length files)))
      (print-group!
       (if (null files)
           (print! (success "All files are up-to-date"))
         (letf! ((defun message (msg &rest args)
                   (when msg (print! (info "%s") (apply #'format msg args)))))
           (cl-dolist (file files)
             (print! (info "Tangling %s...") (file-relative-name file doom-private-dir))
             (+literate-tangle-file file nil 'sync)))
         (with-temp-file cache-file)
         (print! (success "Tangled %s file%s in %.02f seconds")
                 count
                 (if (= count 1) "" "s")
                 (float-time (time-since start)))))
      (unless (or doom-interactive-p (null files))
        (print! (start "Restarting..."))
        (throw 'exit "__NOTANGLE=1 $@")))))

;;;###autoload
(defun +literate-tangle-config-on-save-h ()
  (when (and buffer-file-name (member buffer-file-name +literate-config-files))
    (add-hook 'after-save-hook #'+literate-tangle-file-h nil 'local)))

;;;###autoload
(defalias '+literate/reload #'doom/reload)

;;;###autoload
(add-hook 'org-mode-hook #'+literate-tangle-config-on-save-h)
