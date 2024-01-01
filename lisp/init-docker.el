;;; init-docker.el --- Docker  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package docker
  :defer t)


(use-package dockerfile-mode
  :defer t)


(use-package devdocs
  :hook
  (dockerfile-mode
   . (lambda () (setq-local devdocs-current-docs '("docker")))))


(use-package lsp-mode
  :hook
  (dockerfile-mode . (lambda () (init-lsp-lsp-mode-hook 'dockerfile-ls))))


(provide 'init-docker)
;;; init-docker.el ends here
