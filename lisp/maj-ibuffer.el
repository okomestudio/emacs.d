;;; maj-ibuffer.el --- maj-ibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Ibuffer.
;;
;;; Code:

(use-package ibuffer
  ;; An advanced alternative to `buffer-menu'.
  :straight (:type built-in)
  :init (global-set-key [remap list-buffers] 'ibuffer))

(use-package nerd-icons-ibuffer
  :custom ((nerd-icons-ibuffer-icon t)
           (nerd-icons-ibuffer-icon-size 1.0))
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'maj-ibuffer)
;;; maj-ibuffer.el ends here
