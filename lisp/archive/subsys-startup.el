;;; subsys-startup.el --- Startup Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Emacs startup subsystem.
;;
;;; Code:

(use-package direnv
  ;; direnv integration.
  ;;
  ;; Invoke direnv to obtain the environment for the current file, then update
  ;; the emacs variables process-environment and exec-path.
  ;;
  :ensure-system-package (direnv . "sudo apt install -y direnv")
  :config (direnv-mode))

(provide 'subsys-startup)
;;; subsys-startup.el ends here
