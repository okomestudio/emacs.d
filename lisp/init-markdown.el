;;; init-markdown.el --- Markdown  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Allows browser preview with C-c C-c v
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)

  :ensure-system-package
  ((pandoc . "sudo apt install pandoc"))

  :hook ((markdown-mode) . remove-trailing-whitespaces-on-save)

  :init
  (setq markdown-command "pandoc")

  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.lr\\'" . markdown-mode)))

(provide 'init-markdown)
;;; init-markdown.el ends here
