;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Major mode for the Meson build system for C/C++.
(package! meson-mode)
;; Major mode for simple PHP editing.
(unless (featurep! :lang php) (package! php-mode))

;; Disable escaping insert state when typing too fast.
(package! evil-escape :disable t)
;; Display ^L form-feed as horizontal line.
(package! form-feed)
;; Delete all whitespace in front of point.
(package! hungry-delete)

(when (featurep! :completion company)
  ;; Display function definitions in popup window.
  (package! company-quickhelp)
  ;; Python completion using company and jedi.
  (package! company-jedi))

;; Reformatter for use with :editor/fmt.
(when (featurep! :editor fmt) (package! reformatter))
