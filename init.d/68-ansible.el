;;; 68-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Ansible utilities.
;;
;;; Code:

(use-package ansible ;; not derived from prog-mode
  :hook (ansible . (lambda () (setq-local devdocs-current-docs '("ansible")))))


(use-package poly-ansible
  ;; Combines yaml-mode and jinja2-mode for Ansible.
  ;;
  ;; Add the line
  ;;
  ;;   (auto-mode-alist . (("\\.ya?ml\\'" . poly-ansible-ts-mode)))
  ;;
  ;; to .dir-locals.el for which YAML files are written for Ansible. This
  ;; polymode activate ansible automatically.
  ;;
  ;; jinja2-mode inherits from html-mode.
  ;;
  :demand t
  :config
  ;; Patch poly-ansible to add tree-sitter support:
  (define-hostmode ok-yaml-ts-hostmode
    :mode 'yaml-ts-mode)

  (define-polymode poly-ansible-ts-mode
    :hostmode 'ok-yaml-ts-hostmode
    :innermodes '(pm-inner/jinja2)

    (ansible 1)
    (ansible-doc-mode 1)))


(use-package lsp-mode
  :hook
  ((yaml-mode yaml-ts-mode) . (lambda ()
                                ;; If in ansible-mode, do not activate yamlls.
                                (unless (bound-and-true-p ansible)
                                  (lsp-ensure-server 'yamlls)
                                  (lsp-deferred))))
  (ansible . (lambda ()
               (setq-local lsp-disabled-clients '(yamlls eslint))
               (lsp-ensure-server 'ansible-ls)
               (lsp-deferred)))

  :preface
  (put 'lsp-ansible-python-interpreter-path 'safe-local-variable #'stringp)
  (put 'lsp-ansible-validation-lint-arguments 'safe-local-variable #'stringp))

;;; 68-ansible.el ends here
