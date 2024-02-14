;;; 65-css.el --- css  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure CSS related utilities.
;;
;;; Code:

(use-package css-ts-mode
  :straight nil
  :hook
  (css-ts-mode . (lambda () (setq-local devdocs-current-docs '("css"))))
  (css-ts-mode . lsp))

;;; 65-css.el ends here
