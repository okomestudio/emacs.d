;;; maj-markdown.el --- Markdown Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the major mode for markdown document buffers.
;;
;;; Code:

(use-package markdown-ts-mode
  :bind ( :map markdown-ts-mode-map
          ("C-c C-x v" . markdown-ts-toggle-view-mode)
          :map markdown-ts-view-mode-map
          ("C-c C-x v" . markdown-ts-toggle-view-mode) )
  :mode (("README\\.md\\'" . markdown-ts-mode)
         ("\\.md\\'" . markdown-ts-mode)
         ("\\.markdown\\'" . markdown-ts-mode)
         ("\\.lr\\'" . markdown-ts-mode))
  :config
  (defun markdown-ts-mode--prep ()
    (setq-local fill-column 90)
    (turn-on-visual-line-mode))

  (defun markdown-ts-mode--prep-lsp ()
    "Add this to `markdown-ts-mode-hook' to enable LSP."
    (lsp))

  (defun markdown-ts-toggle-view-mode ()
    "Toggle between `markdown-ts-mode' and `markdown-ts-view-mode'."
    (interactive)
    (if (derived-mode-p 'markdown-ts-view-mode)
        (progn
          (markdown-ts-mode)
          (read-only-mode -1))
      (markdown-ts-view-mode)))

  :hook (markdown-ts-mode . markdown-ts-mode--prep))

(provide 'maj-markdown)
;;; maj-markdown.el ends here
