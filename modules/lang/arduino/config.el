;;; lang/arduino/config.el -*- lexical-binding: t; -*-

(use-package! arduino-mode
  :mode ("\\.ino\\'" "\\.pde\\'")
  :config
  (when (featurep! :checkers syntax)
    (add-hook 'arduino-mode-hook #'flycheck-arduino-setup))
  (map! :map arduino-mode-map
        :localleader
        :desc "Upload Sketch"     "u" #'arduino-upload
        :desc "Compile Sketch"    "c" #'arduino-verify
        :desc "Reset Arduino"     "r" #'arduino-reset
        :desc "Create New Sketch" "n" #'arduino-sketch-new
        :desc "Serial Monitor"    "s" #'arduino-serial-monitor))
