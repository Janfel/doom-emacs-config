;;; lang/basic/config.el -*- lexical-binding: t; -*-

(use-package! basic-mode
  :mode ("\\.bas\\'" "\\.bi\\'")
  :config
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
                   :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))))
