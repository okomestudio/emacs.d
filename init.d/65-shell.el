;;; 65-shell.el --- Shell  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :straight nil

  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)

  :hook
  (sh-mode . flymake-mode)

  :mode
  ("\\.sh\\'" . sh-mode)
  ("bash_*" . sh-mode)
  ("bashrc\\'" . sh-mode)

  ;; NOTE: Commented out in case we need the lines later.
  ;;
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))
  ;; (add-to-list 'interpreter-mode-alist '("bats" . sh-mode))
  )


(use-package flymake
  :ensure-system-package
  (shellcheck . "sudo apt install -y shellcheck"))


(use-package bats-mode)


(use-package ansi-color
  :hook
  (compilation-filter . init-shell--colorize-buffer)

  :config
  (defun init-shell--colorize-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))


(use-package devdocs
  :hook
  (sh-mode . (lambda () (setq-local devdocs-current-docs '("bash")))))


(use-package lsp-mode
  ;; explainshell integration by bash-language-server appears not to be working;
  ;; see github.com/bash-lsp/bash-language-server/issues/726
  :custom
  ;; TODO: Start the service on start.
  ;;
  ;; Run: docker container run --name explainshell --restart always \
  ;;          -p 5023:5000 -d spaceinvaderone/explainshell
  ;;
  (lsp-bash-explainshell-endpoint "http://localhost:5023")
  ;; (lsp-bash-explainshell-endpoint nil)

  (lsp-bash-highlight-parsing-errors t)

  :hook
  (sh-mode . (lambda () (init-lsp-lsp-mode-hook 'bash-ls)))

  :ensure-system-package
  (shellcheck . "sudo apt install -y shellcheck"))

;;; 65-shell.el ends here
