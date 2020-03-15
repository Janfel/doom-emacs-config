;;; editor/fmt/autoload/fmt.el -*- lexical-binding: t; -*-

(require 'dash)

;; Redefine to silence the linter.
(defvar +fmt|prefix-arg)


(defun +fmt--completing-read (&optional mode)
  "Prompt the user for one of the formatters applicable in MODE."
  (let ((formatters (mapcar #'symbol-name (+fmt-get-applicable mode))))
    (intern (completing-read "Formatter: " (sort formatters #'string<)
                             nil t nil nil (car formatters)))))

(defun +fmt--completing-read-all (&optional mode)
  "Prompt the user for one of all formatters.
The initial option is the default formatter for MODE."
    (intern (completing-read
             "Formatter: "
             (sort (mapcar #'symbol-name (+fmt-get-all)) #'string<)
             nil t nil nil (symbol-name (+fmt-get mode)))))

(defun +fmt--read-shell-command (&optional _)
  "Prompt the user for a shell command."
  (read-shell-command "Shell command: "))

(defun +fmt--prompt-fn (arg)
  "Return a function that chooses a formatter based on ARG."
  (when (consp arg)
    (setq arg (car arg)))
  (cond ((eq arg 0) #'+fmt--read-shell-command)
        ((symbolp arg) #'+fmt--completing-read-all)
        ((integerp arg)
         (if (>= arg 0)
             #'+fmt--completing-read
           #'+fmt--completing-read-all))
        (#'+fmt-get)))

(defun +fmt--prefix-arg-value (arg)
  "Return the prefix argument passed to a formatter based on ARG."
  (if (or (null arg) (symbolp arg))
      ;; No prefix argument for invocation with just
      ;; \\[negative-argument] or no prefix at all.
      nil
    (if (integerp arg)
        ;; When invoked with \\[universal-argument] n
        ;; or \\[negative-argument] n where n is some
        ;; integer, return (abs n).
        (abs arg)
      ;; Now ARG must be a cons.
      (setq arg (car arg))
      (if (eq arg 4)
          ;; When invoked with only one \\[universal-argument],
          ;; the user just wants the list of applicable
          ;; formatters.
          nil
        ;; When invoked with more than one \\[universal-argument],
        ;; the user wants to pass that number to the formatter.
        (abs arg)))))

(defun +fmt--run-shell-command-fn (cmd)
  "Return a function that runs the shell command CMD."
  (lambda ()
    (let ((errbuf (generate-new-buffer "temp-shell-command-stderr"))
          (errcode -1)
          (stderr ""))
      (unwind-protect
          (setq errcode (shell-command-on-region nil nil cmd t t errbuf)
                stderr (with-current-buffer errbuf (buffer-string)))
        (kill-buffer errbuf))
      (if (string-empty-p stderr)
          errcode
        (cons (zerop errcode) stderr)))))

(defun +fmt--run-shell-list-fn (shlist arg)
  "Return a function that runs SHLIST with `+fmt|prefix-arg' bound to ARG."
  ;; Bind `+fmt|prefix-arg' in case the symbol appears in SHLIST.
  (let ((+fmt|prefix-arg arg))
    ;; Override `mapconcat' so it can be used as a form in `->>'.
    (cl-flet ((mapconcat (fun seq) (mapconcat fun seq " ")))
      (->> shlist
           (-map #'eval)
           (-flatten-n 1)
           (--map (if (stringp it) it (format "%s" it)))
           (mapconcat #'shell-quote-argument)
           (+fmt--run-shell-command-fn)))))

(defun +fmt--eval-formatter (formatter &optional arg)
  "Return a function or nil."
  (cl-destructuring-bind (fmt . props) (+fmt-lookup formatter)
    (cl-case (plist-get props :type)
      ('function (lambda () (let ((+fmt|prefix-arg arg)) (funcall fmt))))
      ('reference (-> fmt symbol-name (substring 1) intern +fmt--eval-formatter))
      ('sexpr (lambda () (let ((+fmt|prefix-arg arg)) (eval fmt))))
      ('shell-command (+fmt--run-shell-command-fn fmt))
      ('shell-list (+fmt--run-shell-list-fn fmt arg))
      ('alist (-some-> (alist-get arg fmt (alist-get t fmt))
                (+fmt--eval-formatter arg))))))

(defun +fmt--call-formatter (formatter &optional arg)
  "Run the formatting function FORMATTER on the visible buffer.
FORMATTER is a symbol.
ARG is a positive integer."
  (let ((format-fn (+fmt--eval-formatter formatter arg))
        (backup-str (buffer-string))
        result restore)
    (condition-case err
        (setq result (funcall format-fn))
      (error
       (setq restore t)
       (message "Error while formatting: %s" err)))
    (cond ((null result))
          ((consp result)
           (setq restore (not (car result)))
           (apply #'message (cdr result)))
          ((stringp result)
           (erase-buffer)
           (insert result))
          ((and (integerp result)
                (not (zerop result)))
           (setq restore t)
           (message "Return status: %d" result)))
    (when restore
      (erase-buffer)
      (insert backup-str))))


;;; Public library

;;;###autoload
(defun +fmt-buffer (&optional formatter arg)
  "Run FORMATTER with optional ARG on the current buffer.
FORMATTER is a symbol referring to a formatter.
ARG is a positive integer."
  ;; Add `save-excursion' if necessary.
  (+fmt--call-formatter (or formatter (+fmt-get)) arg))

;;;###autoload
(defun +fmt/buffer (arg)
  "Run the active formatter on the active buffer.
Changes behaviour when called with a prefix argument,
see `+fmt/dwim' for defails."
  (declare (interactive-only "Use `+fmt-buffer' instead."))
  (interactive "P")
  (+fmt-buffer
   (funcall (+fmt--prompt-fn arg) major-mode)
   (+fmt--prefix-arg-value arg)))

;;;###autoload
(defun +fmt-region (beg end &optional formatter arg)
  (if (eq beg end)
      ;; Should this be an error?
      (+fmt-buffer formatter arg)
    (if (> beg end) (cl-rotatef beg end))
    (save-excursion
      (save-restriction
        ;; Normalize the region to use full lines.
        (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
        (setq beg (max beg (point)))
        (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
        (setq end (min end (point)))
        (if (>= beg end) (error "Nothing to format"))

        (narrow-to-region beg end)
        (cl-letf (((symbol-function 'widen) #'ignore))
          (+fmt-buffer formatter arg))))))

;;;###autoload
(defun +fmt/region (beg end arg)
  "Run the active formatter on the selected region.
Changes behaviour when called with a prefix argument,
see `+fmt/dwim' for defails."
  (declare (interactive-only "Use `+fmt-region' instead."))
  (interactive "r\nP")
  (+fmt-region
   beg end
   (funcall (+fmt--prompt-fn arg) major-mode)
   (+fmt--prefix-arg-value arg)))

;;;###autoload
(defun +fmt/dwim ()
  "Run the active formatter on the buffer or selected region.

When called without a prefix argument, runs the default formatter for this
major mode. When called with a positive prefix argument, lets the user choose
one of the formatters registered for this major mode. When called with a
negative prefix argument, lets the user choose one of all registered formatters.
When called with a prefix argument that is not produced by \\[universal-argument]
or \\[negative-argument], the absolute numeric value of this prefix argument
will be made available to the formatter."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'+fmt/region
     #'+fmt/buffer)))
