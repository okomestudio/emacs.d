;;; init-file-type-modes.el --- File type modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package prog-mode
  :straight nil
  :hook
  (prog-mode . (lambda ()
                 (add-hook 'local-write-file-hooks
                           #'(lambda () (save-excursion
                                          (delete-trailing-whitespace))))
                 (show-paren-mode))))


(use-package text-mode
  :straight nil
  :hook
  (text-mode . (lambda ()
                 (add-hook 'local-write-file-hooks
                           #'(lambda () (save-excursion
                                          (delete-trailing-whitespace)))))))


;; INI

(use-package any-ini-mode
  :disabled

  ;; :init
  ;; (require 'okutil)
  ;; (okutil-ensure-file-from-url
  ;;  "https://www.emacswiki.org/emacs/download/any-ini-mode.el")

  :mode
  "\\.ini\\'" "\\.conf\\'")


;; MARKDOWN

(use-package markdown-mode
  ;; For browser preview, use C-c C-c v.
  :custom
  (markdown-fontify-code-block-natively t)
  (markdown-header-scaling t)
  (markdown-indent-on-enter t)

  :commands (markdown-mode gfm-mode)

  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode)
   ("\\.lr\\'" . gfm-mode))

  :hook
  (markdown-mode . lsp)

  :ensure-system-package
  (marksman . "sudo snap install marksman")
  (pandoc . "sudo apt install -y pandoc")

  :init
  (setq markdown-command "pandoc")

  :config
  (require 'lsp-marksman))


;; RST

(use-package rst-mode
  :straight nil
  :ensure-system-package ((sphinx-quickstart . "pip install sphinx"))
  :mode "\\.rst\\'")


;; SSH CONFIG

(use-package ssh-config-mode)


;; SYSTEMD

(use-package systemd
  :mode ("\\.\\(service|timer\\)\\'" . systemd-mode))


;; YAML

(use-package yaml-mode
  :hook (yaml-mode . (lambda () (typo-mode -1)))
  :mode ("\\.ya?ml\\(\\.j2\\)?\\'" . yaml-mode))


(provide 'init-file-type-modes)
;;; init-file-type-modes.el ends here
