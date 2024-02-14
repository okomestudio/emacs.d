;;; 65-html.el --- html  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure HTML related utilities.
;;
;;; Code:

(use-package mhtml-mode
  :straight nil
  :hook
  (css-ts-mode . (lambda () (setq-local devdocs-current-docs '("html"))))
  (mhtml-mode . lsp))

;;; 65-html.el ends here
