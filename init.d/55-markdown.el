;;; 55-markdown.el --- Markdown  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  ;; For browser preview, use C-c C-c v.
  :custom
  (markdown-fontify-code-block-natively t)
  (markdown-header-scaling t)
  (markdown-indent-on-enter t)

  :commands
  (markdown-mode gfm-mode)

  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . gfm-mode)
  ("\\.markdown\\'" . gfm-mode)
  ("\\.lr\\'" . gfm-mode)

  :hook
  (markdown-mode . lsp)

  :ensure-system-package
  (pandoc . "sudo apt install -y pandoc")

  ;; :init
  ;; (setq markdown-command "pandoc")

  :config
  (require 'lsp-marksman))


(use-package devdocs
  :hook
  (markdown-mode . (lambda () (setq-local devdocs-current-docs '("markdown")))))


(use-package lsp-mode
  :hook
  (markdown-mode . lsp))

;;; 55-markdown.el ends here
