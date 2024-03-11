;;; 57-conf-mode.el --- conf-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure conf-mode and related utilities.
;;
;;; Code:

(use-package conf-mode
  :disabled
  :straight nil
  ;; :init
  ;; (ok-file-ensure-from-url
  ;;  "https://www.emacswiki.org/emacs/download/any-ini-mode.el")

  :mode
  ("\\.ini\\'"
   "\\.conf\\'"))


(use-package git-modes
  ;; Major modes for .git(attributes|config|ignore)
  )


(use-package systemd
  :mode ("\\.\\(service|timer\\)\\'" . systemd-mode))


;; Config modes not derived from `conf-mode'

(use-package ssh-config-mode)

;;; 57-conf-mode.el ends here
