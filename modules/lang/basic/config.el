;;; lang/basic/config.el -*- lexical-binding: t; -*-

(use-package! basic-mode
  :mode ("\\.bas\\'" "\\.bi\\'")
  :config
  ;; Syntax rules for FreeBASIC block comments.
  (modify-syntax-entry ?' "< 23"   basic-mode-syntax-table)
  (modify-syntax-entry ?/ "_ 14cn" basic-mode-syntax-table)
  (map! :map basic-mode-map :localleader
        :desc "Renumber lines" "r" #'basic-renumber)
  (after! smartparens
    (sp-local-pair 'basic-mode "'" nil :actions nil)
    (sp-local-pair 'basic-mode "/'" "'/"
                   :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))))
