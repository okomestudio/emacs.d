;;; maj-conf-mode.el --- Conf Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Conf major mode.
;;
;;; Code:

(use-package conf-mode)

(use-package git-modes
  ;; Major modes for .git(attributes|config|ignore)
  )

(use-package systemd
  :mode ("\\.\\(service|timer\\)\\'" . systemd-mode))

;;; Config modes not derived from `conf-mode'

(use-package ssh-config-mode)

(provide 'maj-conf-mode)
;;; maj-conf-mode.el ends here
