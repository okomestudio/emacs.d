;;; markdown.el --- Markdown  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Markdown configuration.
;;
;; NOTE: tree-sitter support exists, but not as mature.
;;
;;; Code:

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("\\.lr\\'" . gfm-mode))
  :bind (;
         :map markdown-mode-map
         ("C-c C-c v". markdown-export-and-preview))
  :custom ((markdown-fontify-code-block-natively t)
           (markdown-header-scaling t)
           (markdown-indent-on-enter t))
  :hook (markdown-mode . lsp))

;;; markdown.el ends here
