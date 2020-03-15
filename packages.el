;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;;; Major Modes:

;; Major-mode for Arduino sketches.
(package! arduino-mode)

;; Major-mode for the Meson build system for C/C++.
(package! meson-mode)

;; Major-mode for PHP for when (:lang php) is disabled.
(unless (featurep! :lang php)
  (package! php-mode))

;;; Minor Modes:

;; Disable escaping insert state when typing too fast.
(package! evil-escape :disable t)

;; Display ^L form-feed as horizontal line.
(package! form-feed)

;; Delete all whitespace in front of point.
(package! hungry-delete)

;;; Miscellaneous:

;; Python completion using company and jedi.
(when (and (featurep! :completion company)
           (not (featurep! :lang python +lsp)))
  (package! company-jedi))
