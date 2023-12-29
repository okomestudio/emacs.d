;;; init-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package ansible)


(use-package devdocs
  :hook
  (ansible . (lambda () (setq-local devdocs-current-docs '("ansible")))))


(use-package lsp-mode
  :hook
  (ansible . (lambda ()
               (lsp-disconnect)         ; disconnect yamlls
               (setq-local lsp-disabled-clients '(yamlls))
               (init-lsp-lsp-mode-hook 'ansible-ls))))


(provide 'init-ansible)
;;; init-ansible.el ends here
