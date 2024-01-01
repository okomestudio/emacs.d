;;; init-conf-mode.el --- conf-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package conf-mode
  :disabled
  :straight nil

  ;; :init
  ;; (require 'okutil)
  ;; (okutil-ensure-file-from-url
  ;;  "https://www.emacswiki.org/emacs/download/any-ini-mode.el")

  :mode
  ("\\.ini\\'"
   "\\.conf\\'"))


;; SSH CONFIG

(use-package ssh-config-mode) ;; doesn't actually derive from conf-mode


;; SYSTEMD

(use-package systemd
  :mode ("\\.\\(service|timer\\)\\'" . systemd-mode))


(provide 'init-conf-mode)
;;; init-conf-mode.el ends here
