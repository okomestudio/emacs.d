;;; css.el --- CSS  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; CSS mode configuration.
;;
;;; Code:

(use-package css-ts-mode
  :straight nil
  :hook (css-ts-mode . lsp)
  :custom (css-indent-offset 2))

;;; css.el ends here
