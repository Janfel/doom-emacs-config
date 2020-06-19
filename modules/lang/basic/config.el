;;; lang/basic/config.el -*- lexical-binding: t; -*-

(use-package! basic-mode
  :mode ("\\.bas\\'" "\\.bi\\'")
  :config
  ;; Syntax rules for FreeBASIC block comments.
  (modify-syntax-entry ?' "< 23"   basic-mode-syntax-table)
  (modify-syntax-entry ?/ "_ 14cn" basic-mode-syntax-table)

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
