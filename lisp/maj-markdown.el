;;; maj-markdown.el --- Markdown Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the markdown major mode.
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
  :bind (:map markdown-mode-map
              ("C-c C-c v" . markdown-export-and-preview))
  :custom ((markdown-fontify-code-block-natively t)
           (markdown-header-scaling t)
           (markdown-indent-on-enter t))
  :hook (markdown-mode . markdown-mode-ok--init)
  :config
  (defun markdown-mode-ok--init ()
    (setq-local fill-column 90)
    (turn-on-visual-line-mode)
    (lsp)))

(provide 'maj-markdown)
;;; maj-markdown.el ends here
