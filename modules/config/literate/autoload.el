;;; config/literate/autoload.el -*- lexical-binding: t; -*-

(defvar +literate-config-files
  (list (expand-file-name "config.org" doom-private-dir))
  "A list of `org-mode' files to be tangled automatically.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file to serve as mtime cache for tangling `+literate-config-files'.")

(defvar org-mode-hook)
(defvar org-inhibit-startup)


;;;###autoload
(defun +literate-tangle-sync-h ()
  "Tangles `+literate-config-file' if it has changed."
  (unless (getenv "NOTANGLE")
    (print! (start "Compiling your literate config..."))
    (print-group!
     (require 'ob-tangle)
     (require 'ox)
     (let* ((start (current-time))
            (targets
             (seq-filter
              (lambda (f) (file-newer-than-file-p f +literate-config-cache-file))
              +literate-config-files)))
       (let ((cache +literate-config-cache-file)
             ;; Prevent unwanted entries in recentf, or formatters, or
             ;; anything that could be on these hooks, really. Nothing
             ;; else should be touching these files (particularly in
             ;; interactive sessions).
             (write-file-functions nil)
             (before-save-hook nil)
             (after-save-hook nil)
             ;; Prevent infinite recursion due to recompile-on-save
             ;; hooks later, and speed up `org-mode' init.
             (org-mode-hook nil)
             (org-inhibit-startup t))
         ;; Ensure output conforms to the formatting of all doom CLIs
         (letf! ((defun message (msg &rest args)
                   (when msg (print! (info "%s") (apply #'format msg args)))))
           (cl-dolist (target targets)
             (set-buffer (find-file-noselect target))
             ;; Tangling won't ordinarily expand #+INCLUDE directives,
             ;; so I do it myself.
             (org-export-expand-include-keyword)
             (org-babel-tangle)))
         (with-temp-file cache)
         (if (zerop (length targets))
             (print! (success "No files to tangle"))
           (print! (success "Tangled %s file(s) in %.02f seconds"
                            (length targets)
                            (float-time (time-since start))))))))))

;;;###autoload
(defun +literate-tangle-async-h ()
  "Tangle `+literate-config-files' asynchronously.
Use this function when in interactive mode."
  (require 'async)
  (let* ((start (current-time))
         (files
          (seq-filter
           (lambda (f) (file-newer-than-file-p f +literate-config-cache-file))
           +literate-config-files))
         (counter (length files)))
    (cl-dolist (file files)
      (async-start
       (lambda ()
         (require 'ob-tangle)
         (require 'ox)
         (let ((org-mode-hook nil)
               (org-inhibit-startup t))
           (set-buffer (find-file-noselect file))
           (org-export-expand-include-keyword)
           (org-babel-tangle nil file)))
       (lambda (_)
         (when (zerop (cl-decf counter))
           (with-temp-file +literate-config-cache-file)
           (message "Tangling %s finished in %.02f seconds"
                    (mapcar #'file-name-nondirectory files)
                    (float-time (time-since start)))))))))

;;;###autoload
(defun +literate-maybe-enable-tangle-on-save-h ()
  "Enable tangling on save in `+literate-config-files'."
  (when (and buffer-file-name (member buffer-file-name +literate-config-files))
    (add-hook 'after-save-hook #'+literate-tangle-async-h nil 'local)))

;;;###autoload
(defalias '+literate/reload #'doom/reload)

;;;###autoload
(add-hook 'org-mode-hook #'+literate-maybe-enable-tangle-on-save-h)
