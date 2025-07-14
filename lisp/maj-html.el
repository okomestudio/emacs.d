;;; maj-html.el --- HTML Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the HTML major mode.
;;
;;; Code:

(use-package mhtml-mode
  :hook (mhtml-mode . lsp))

(use-package emmet-mode
  ;; Add HTML/CSS abbreviation expansion.
  :disabled)

(provide 'maj-html)
;;; maj-html.el ends here
