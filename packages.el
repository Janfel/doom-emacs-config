;;; packages.el -*- no-byte-compile: t; -*-

;; The elided SHA1 hashes are caused by `+emacs-lisp-truncate-pin'.

;; Major mode for Meson build files.
(package! meson-mode)
;; My own verson of `moonscript-mode'.
(package! moonscript
  :pin "ac5d35d3a1970db958c687991a1f2c49f7921150"
  :recipe (:fork (:host github :repo "Janfel/moonscript-mode")))
;;(package! moonscript :recipe (:local-repo "/home/janfel/src/elisp/moonscript-mode"))
;; Major mode for simple PHP editing.
(unless (featurep! :lang php) (package! php-mode))

;; Library for running async processes.
(package! async)
;; Disable escaping insert state when typing too fast.
(package! evil-escape :disable t)
;; Improve jumping with %.
(package! evil-matchit)
;; Clang-Tidy integration for flycheck.
(package! flycheck-clang-tidy)
;; Display ^L form-feed as horizontal line.
(package! form-feed)
;; Display each indentation level.
;;(package! highlight-indent-guides)
;; Delete all whitespace in front of point.
(package! hungry-delete)
;; Open Intel x86 Developer Manual on x86 commands.
;;(package! x86-lookup)
;; Disable `latex-preview-pane'.
(package! latex-preview-pane :disable t)

(when (featurep! :completion company)
  ;; Display function definitions in popup window.
  (package! company-quickhelp)
  ;; Python completion using company and jedi.
  (package! company-jedi))
