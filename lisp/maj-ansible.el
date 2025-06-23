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

(use-package ansible-doc
  :bind ( :map help-map
          ("A" . ansible-doc)
          :map ansible-doc-module-mode-map
          ("r" . ok-ansible-doc--redraw) )
  :hook ((yaml-ts-mode . ansible-doc-mode))
  :config
  ;; NOTE(2025-06-22): Ideally, this is run right after `ansible-doc'.
  ;; Attempts have been made to add this to a hook or an advice.
  ;; However, the coordination of redraw after
  ;; `ansi-color-apply-on-region' seems tricky, and aside from
  ;; repeating `ansible-doc' invocation, page refresh doesn't happen
  ;; properly. Revisit to fix this issue.
  (defun ok-ansible-doc--redraw ()
    "Redraw page after processing ASCII escapes."
    (interactive)
    (with-silent-modifications
      (ansi-color-apply-on-region (point-min) (point-max) t))
    (redisplay t)))

(provide 'maj-ansible)
;;; maj-ansible.el ends here
