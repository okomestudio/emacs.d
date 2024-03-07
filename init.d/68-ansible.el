;;; 68-ansible.el --- ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Ansible and related utilities.
;;
;;; Code:

(use-package ansible ;; not derived from prog-mode
  :hook (ansible . ansible--prepare-lsp)
  :preface
  (put 'lsp-ansible-python-interpreter-path 'safe-local-variable #'stringp)
  (put 'lsp-ansible-validation-lint-arguments 'safe-local-variable #'stringp)

  :config
  (defun ansible--prepare-lsp ()
    (lsp-booster-mode -1)
    ;; NOTE: To only run ansible-ls, uncomment the following line:
    (setq-local lsp-disabled-clients '(yamlls))
    (lsp-deferred)))

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
  :commands poly-ansible-ts-mode
  :straight (:host github :repo "okomestudio/poly-ansible"
                   :branch "ts-mode" :fork "okomestudio"))

;;; 68-ansible.el ends here
