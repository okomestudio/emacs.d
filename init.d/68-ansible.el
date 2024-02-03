;;; 68-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ansible :defer t) ;; not derived from prog-mode


(use-package poly-ansible
  ;; Combines yaml-mode and jinja2-mode for Ansible.
  ;;
  ;; Add the line
  ;;
  ;;   (auto-mode-alist . (("\\.ya?ml\\'" . poly-ansible-mode)))
  ;;
  ;; to .dir-locals.el for which YAML files are written for Ansible. This
  ;; polymode activate ansible automatically.
  ;;
  ;; jinja2-mode inherits from html-mode.
  :defer t
  )


(use-package devdocs
  :defer t

  :hook
  (ansible . (lambda () (setq-local devdocs-current-docs '("ansible")))))


(use-package lsp-mode
  :defer t

  :hook
  (ansible . (lambda ()
               (setq-local lsp-disabled-clients '(yamlls eslint))
               (lsp-ensure-server 'ansible-ls)
               (lsp-deferred)))

  :preface
  (put 'lsp-ansible-python-interpreter-path 'safe-local-variable #'stringp)
  (put 'lsp-ansible-validation-lint-arguments 'safe-local-variable #'stringp))

;;; 68-ansible.el ends here
