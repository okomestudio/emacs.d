;;; maj-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the YAML major mode for use with Ansible.
;;
;;; Code:

(use-package ansible
  :hook (ansible . ansible-ok--lsp-deferred)
  :config
  (defun ansible-ok--lsp-deferred ()
    "Defer LSP activation."
    ;; Uncomment to disable `yamlls'.
    ;; (setq-local lsp-disabled-clients '(yamlls))
    (lsp-deferred)))

(use-package poly-ansible
  ;; Combines `yaml-mode' and `jinja2-mode' for use in Ansible.
  ;;
  ;; Add the line
  ;;
  ;;   (auto-mode-alist . (("\\.ya?ml\\'" . poly-ansible-mode)))
  ;;
  ;; to .dir-locals.el of the project in which YAML files are written
  ;; for Ansible. This polymode activates ansible automatically.
  ;;
  ;; Note that `poly-ansible-mode' activates `yaml-ts-mode' if that is
  ;; in use and falls back to `yaml-mode' if not.
  ;;
  ;; `jinja2-mode' inherits from html-mode.
  )

(provide 'maj-ansible)
;;; maj-ansible.el ends here
