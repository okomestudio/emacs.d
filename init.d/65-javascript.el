;;; 65-javascript.el --- javascript  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure JavaScript related utilities.
;;
;;; Code:

(use-package js-ts-mode
  :straight nil
  :hook
  (js-ts-mode . (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (js-ts-mode . lsp))

;;; 65-javascript.el ends here
