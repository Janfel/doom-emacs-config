;; -*- no-byte-compile: t; -*-
;;; lang/basic/packages.el

(package! basic-mode)
(package! fbasic-mode
  :type 'local
  :recipe '(:local-repo "/home/janfel/src/elisp/fbasic-mode/"
            ;; To prevent code caching during development.
            :no-byte-compile t))
