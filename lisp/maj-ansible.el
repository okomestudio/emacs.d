;;; maj-ansible.el --- Ansible Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Ansible major mode.
;;
;;; Code:

(require 'ok)

(use-package ansible ;; not derived from prog-mode
  :hook (ansible . ansible--prepare-lsp)
  :config
  (defun ansible--prepare-lsp ()
    ;; NOTE(2024-03-07): `emacs-lsp-booster' is not compatible with
    ;; `ansible-ls' v1.2.1 (see
    ;; github.com/blahgeek/emacs-lsp-booster/issues/18).
    ;;
    ;; This is fixed by ansible/ansible-language-server/pull/604,
    ;; which is not released yet. Until the new release, either turn
    ;; off `emacs-lsp-booster' or patch `ansible-ls'. To patch, clone
    ;; github.com/ansible/ansible-language-server, build the server
    ;; locally following
    ;; ansible.readthedocs.io/projects/language-server/development/#building-server-locally,
    ;; and replace the "out" directory, i.e.,
    ;; "var/lsp/server/npm/\@ansible/ansible-language-server/lib/node_modules/\@ansible/ansible-language-server/out/"
    ;; with the local build.

    ;; Uncomment the following line to disable `emacs-lsp-booster':
    ;; (lsp-booster-mode -1)

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

(provide 'maj-ansible)
;;; maj-ansible.el ends here
