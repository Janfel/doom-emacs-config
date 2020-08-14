;;; lang/meson/config.el -*- lexical-binding: t; -*-

(use-package! meson-mode
  :defer t
  :config
  (set-lookup-handlers! 'meson-mode
    :documentation #'+meson-lookup-doc))
