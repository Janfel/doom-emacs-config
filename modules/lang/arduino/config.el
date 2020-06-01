;;; lang/arduino/config.el -*- lexical-binding: t; -*-

(use-package! arduino-mode
  :mode ("\\.ino\\'" "\\.pde\\'")
  :config
  (advice-add #'arduino-serial-monitor :around #'+arduino-use-popup-system-a)
  ;; Buffers created by `serial-term' don’t have a common prefix.
  (set-popup-rule! "^/dev/tty\\(ACM\\|S\\|USB\\)[0-9]+$" :ttl nil :select nil)
  (map! :map arduino-mode-map
        :localleader
        :desc "Create New Sketch" "n" #'arduino-sketch-new
        :desc "Compile Sketch"    "c" #'arduino-verify
        :desc "Upload Sketch"     "u" #'arduino-upload
        :desc "Serial Monitor"    "s" #'arduino-serial-monitor
        :desc "Reset Arduino"     "r" #'arduino-reset)
  (when (featurep! :checkers syntax)
    (add-hook 'arduino-mode-hook #'flycheck-arduino-setup)))
