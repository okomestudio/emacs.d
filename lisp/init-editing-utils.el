;;; init-editing-utils.el --- Editing-Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Switch between the beginning/end of line or codeLine positioning
(use-package mwim
  :bind
  (("C-a" . 'mwim-beginning)
   ("C-e" . 'mwim-end)))

;; Convert buffer text and decorations to HTML
(use-package htmlize)

;; Titlecase things in Emacs
;;
;; https://github.com/duckwork/titlecase.el
(use-package titlecase)

;; Typographical utility (e.g., smart quotation)
(use-package typo
  :hook ((text-mode . typo-mode)))

;; Treat undo history as a tree
(use-package undo-tree
  :init (global-undo-tree-mode))

;; Operate on current line if region undefined
(use-package whole-line-or-region)

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
