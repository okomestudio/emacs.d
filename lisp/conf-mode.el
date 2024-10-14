;;; conf-mode.el --- conf-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The conf-mode configuration.
;;
;;; Code:

(use-package conf-mode
  :straight nil)

(use-package git-modes
  ;; Major modes for .git(attributes|config|ignore)
  )

(use-package systemd
  :mode ("\\.\\(service|timer\\)\\'" . systemd-mode))

;; Config modes not derived from `conf-mode'

(use-package ssh-config-mode)

;;; conf-mode.el ends here
