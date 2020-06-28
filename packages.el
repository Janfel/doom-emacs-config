;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Major mode for the Meson build system for C/C++.
(package! meson-mode)
;; My own verson of `moonscript-mode'.
(package! moonscript :recipe (:host github :repo "Janfel/moonscript-mode"))
;;(package! moonscript :recipe (:local-repo "/home/janfel/src/elisp/moonscript-mode"))
;; Major mode for simple PHP editing.
(unless (featurep! :lang php) (package! php-mode))

;; Library for running async processes.
(package! async)
;; Disable escaping insert state when typing too fast.
(package! evil-escape :disable t)
;; Unpin flycheck. The version Doom currently ships is broken for HLint.
(package! flycheck :pin nil)
;; Display ^L form-feed as horizontal line.
(package! form-feed)
;; Display each indentation level.
(package! highlight-indent-guides)
;; Delete all whitespace in front of point.
(package! hungry-delete)

(when (featurep! :completion company)
  ;; Display function definitions in popup window.
  (package! company-quickhelp)
  ;; Python completion using company and jedi.
  (package! company-jedi))
