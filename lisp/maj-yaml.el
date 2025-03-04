;;; maj-yaml.el --- YAML Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the YAML major mode.
;;
;;; Code:

(use-package yaml-ts-mode
  :straight nil
  :mode "\\.ya?ml\\'"
  :hook ((yaml-ts-mode . yaml-ok--patch-yaml-ts-mode)
         (yaml-ts-mode . lsp-deferred))
  :config
  (defun yaml-ok--patch-yaml-ts-mode ()
    "Patch `yaml-ts-mode'."
    ;; The built-in `yaml-ts-mode' does not implement much of
    ;; anything. This patch loads variables from `yaml-mode' and
    ;; expose the features in `yaml-ts-mode'. See what `yaml-mode'
    ;; derived mode definition does in `yaml-mode.el'.
    (require 'yaml-mode)
    (set (make-local-variable 'comment-start) "# ")
    (set (make-local-variable 'comment-start-skip) "#+ *")
    (set (make-local-variable 'indent-line-function) 'yaml-indent-line)
    (set (make-local-variable 'indent-tabs-mode) nil)
    (set (make-local-variable 'fill-paragraph-function) 'yaml-fill-paragraph)
    (set (make-local-variable 'syntax-propertize-function)
         'yaml-mode-syntax-propertize-function)
    (setq-local font-lock-defaults '(yaml-font-lock-keywords))))

(use-package jq-mode
  :after yaml-ts-mode
  :bind (:map
         yaml-ts-mode-map
         ("C-c C-j" . jq-interactively))
  :config
  (defun jq-interactively-on-yaml (&rest r)
    (when (derived-mode-p 'yaml-ts-mode)
      (setq-local jq-interactive-command (ok-file-expand-bin "yq")
                  jq-interactive-font-lock-mode #'yaml-ts-mode
                  jq-interactive-default-options "--yaml-roundtrip")))
  (advice-add #'jq-interactively :before #'jq-interactively-on-yaml))

(provide 'maj-yaml)
;;; maj-yaml.el ends here
