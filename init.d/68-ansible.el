;;; 68-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ansible) ;; not derived from prog-mode


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
  )


(use-package devdocs
  :hook
  (ansible . (lambda () (setq-local devdocs-current-docs '("ansible")))))


(use-package lsp-mode
  :hook
  (yaml-mode . (lambda ()
                 ;; If in ansible-mode, do not activate yamlls.
                 (if (not (bound-and-true-p ansible))
                     (init-lsp-lsp-mode-hook 'yamlls))))
  (ansible . (lambda ()
               (setq-local lsp-disabled-clients '(yamlls eslint))
               (lsp-ensure-server 'ansible-ls)
               (lsp-deferred)))

  :preface
  (put 'lsp-ansible-python-interpreter-path 'safe-local-variable #'stringp)
  (put 'lsp-ansible-validation-lint-arguments 'safe-local-variable #'stringp))

;;; 68-ansible.el ends here
