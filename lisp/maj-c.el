;;; maj-c.el --- C Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the C major mode.
;;
;;; Code:

(use-package cc-mode
  :straight (:type built-in)
  :config
  (setopt c-basic-offset 2))

(provide 'maj-c)
;;; maj-c.el ends here
