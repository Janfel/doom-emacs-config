;;; lang/basic/config.el -*- lexical-binding: t; -*-

(use-package! basic-mode
  :mode ("\\.bas\\'" "\\.bi\\'")
  :config
  (defun +basic-denumber (beg end)
    "Remove line number from every line in the region BEG to END."
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (basic-remove-line-number)
        (forward-line 1))))

  (defun +basic/denumber ()
    "Remove line number from every line in current buffer/region."
    (interactive "*")
    (if (doom-region-active-p)
        (+basic-denumber (doom-region-beginning) (doom-region-end))
      (+basic-denumber (point-min) (point-max))))
  ;; Syntax rules for FreeBASIC block comments.
  (modify-syntax-entry ?' "< 23"   basic-mode-syntax-table)
  (modify-syntax-entry ?/ "_ 14cn" basic-mode-syntax-table)

  (defconst basic-increase-indent-keywords-bol
    (regexp-opt '("do" "for" "function" "repeat" "sub" "while")
                'symbols))
  (defconst basic-keyword-regexp
    (regexp-opt '("as" "call" "const" "declare" "def" "defbol" "defdbl" "defint"
                  "defsng" "defstr" "dim" "do" "else" "elseif" "end" "endif"
                  "error" "exit" "fn" "for" "function" "gosub" "goto" "if"
                  "loop" "next" "on" "step" "repeat" "return" "shared" "sub"
                  "then" "to" "until" "wend" "while")
                'symbols))
  (defconst basic-type-regexp
    (regexp-opt '("any" "boolean" "byte" "double" "integer" "longint"
                  "pointer" "ptr" "short" "single" "string" "ubyte"
                  "uinteger" "ulongint" "ushort" "zstring")
                'symbols))
  (defconst basic-builtin-regexp
    (regexp-opt '("and" "cls" "data" "draw" "input" "let" "mat" "mod" "not" "or"
                  "peek" "poke" "print" "read" "restore" "troff" "tron" "xor")
                'symbols))
  (defconst basic-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]*")
  (defconst basic-font-lock-keywords
    (list (list basic-comment-regexp 0 'font-lock-comment-face)
          (list basic-linenum-regexp 0 'font-lock-constant-face)
          (list basic-label-regexp 0 'font-lock-constant-face)
          (list basic-constant-regexp 0 'font-lock-constant-face)
          (list basic-keyword-regexp 0 'font-lock-keyword-face)
          (list basic-type-regexp 0 'font-lock-type-face)
          (list basic-function-regexp 0 'font-lock-function-name-face)
          (list basic-builtin-regexp 0 'font-lock-builtin-face)
          (list basic-preprocessor-regexp 0 'font-lock-preprocessor-face)))


  (map! :map basic-mode-map "C-c C-d" #'+basic/denumber)
  (map! :map basic-mode-map :localleader
        :desc "Renumber lines" "r" #'basic-renumber
        :desc "Delete line numbers" "d" #'+basic/denumber)

  (defadvice! +basic-indent-line ()
    "Just go to `line-end-position' instead of calculating something weird."
    :override #'basic-indent-line
    ;; "Indent the current line of code, see function `basic-calculate-indent'."
    (interactive)
    ;; If line needs indentation
    (when (or (not (basic-line-number-indented-correctly-p))
              (not (basic-code-indented-correctly-p)))
      ;; Set basic-line-number-cols to reflect the actual code
      (let* ((actual-line-number-cols
              (if (not (basic-has-line-number-p))
                  0
                (let ((line-number (basic-current-line-number)))
                  (1+ (length (number-to-string line-number))))))
             (basic-line-number-cols
              (max actual-line-number-cols basic-line-number-cols)))
        ;; Calculate new indentation
        (let* ((original-col (- (current-column) basic-line-number-cols))
               (original-indent-col (basic-current-indent))
               (calculated-indent-col (basic-calculate-indent)))
          (basic-indent-line-to calculated-indent-col)
          ;; Just go to `line-end-position' instead of calculating something.
          (goto-char (line-end-position))))))

  (defadvice! +basic--current-indent-respect-tabs-a (&rest _)
    "Make `basic-current-indent' calculate the correct indent when using tabs."
    :override #'basic-current-indent
    (beginning-of-line)
    (save-restriction
      (narrow-to-region
       (or (re-search-forward "^[ \t]*[0-9]+[ \t]" (line-end-position) t)
           (point))
       (line-end-position))
      (current-indentation)))

  (defadvice! +basic--retab-after-formatting-a (&rest _)
    "Call `doom/retab' after `basic-format-code' to avoid mixing tabs and spaces."
    :after #'basic-format-code
    (if (region-active-p)
        (doom/retab nil (region-beginning) (region-end))
      (doom/retab nil)))

  (after! smartparens
    (sp-local-pair 'basic-mode "'" nil :actions nil)
    (sp-local-pair 'basic-mode "/'" "'/"
                   :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

  (after! highlight-numbers
    (puthash 'basic-mode
             (rx (? (or "+" "-"))
                 (or (seq (or (+ digit)
                              (seq "&H" (+ xdigit))
                              (seq "&O" (+ (any "0-7")))
                              (seq "&B" (+ (or "0" "1"))))
                          (? (or "%" "U" "L" "&" "UL" "LL" "ULL")))
                     (seq (or (seq (+ digit) (? "." (* digit)))
                              (seq "." (+ digit)))
                          (? (or "D" "E") (? (or "+" "-")) (* digit))
                          (? (or "!" "F" "#" "D")))))
             highlight-numbers-modelist)))
