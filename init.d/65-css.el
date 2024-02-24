;;; 65-css.el --- css  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure CSS related utilities.
;;
;;; Code:

(use-package css-ts-mode
  :straight nil
  :hook (css-ts-mode . lsp)
  :custom (css-indent-offset 2))

;;; 65-css.el ends here
