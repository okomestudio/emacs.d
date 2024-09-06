;;; html.el --- html  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; HTML mode configuration.
;;
;;; Code:

(use-package mhtml-mode
  :straight nil
  :hook
  (mhtml-mode . lsp))

(use-package emmet-mode
  ;; Add HTML/CSS abbreviation expansion.
  :disabled)

;;; html.el ends here
