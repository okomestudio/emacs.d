;;; init-windows.el --- Windows-related packages  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; shackle - Enforce rules for popup windows
;; https://depp.brause.cc/shackle/
(use-package shackle
  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 0.4)
  ;; (shackle-rules '(("*Warnings*"
  ;;                   :select nil :size 0.25)
  ;;                  (magit-status-mode
  ;;                   :align right :size 0.5 :inhibit-window-quit t :other t)))

  :config
  (shackle-mode 1))

(provide 'init-windows)
;;; init-windows.el ends here
