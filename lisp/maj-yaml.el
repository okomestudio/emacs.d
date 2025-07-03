;;; maj-yaml.el --- YAML Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the YAML major mode.
;;
;;; Code:

(use-package yaml-mode)

(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode "\\.ya?ml\\'"
  :hook ((yaml-ts-mode . yaml-ts-mode-ok--patch)
         (yaml-ts-mode . lsp-deferred))
  :config
  (defun yaml-ts-mode-ok--patch ()
    "Patch `yaml-ts-mode'.
The built-in `yaml-ts-mode' does not implement all the features
available in `yaml-mode'. This patch exposes in `yaml-ts-mode' the
variables defined in `yaml-mode' (see the `define-derived-mode'
section)."
    (require 'yaml-mode)
    (set (make-local-variable 'comment-start) "# ")
    (set (make-local-variable 'comment-start-skip) "#+ *")
    (set (make-local-variable 'comment-end) "")
    (set (make-local-variable 'indent-line-function) 'yaml-indent-line)
    (set (make-local-variable 'syntax-propertize-function)
         #'yaml-mode-syntax-propertize-function)))

(use-package jq-mode
  :after yaml-ts-mode
  :bind ( :map yaml-ts-mode-map
          ("C-c C-j" . jq-interactively) )
  :config
  (defun jq-interactively-on-yaml (&rest r)
    (when (derived-mode-p 'yaml-ts-mode)
      (setq-local jq-interactive-command (ok-file-expand-bin "yq")
                  jq-interactive-font-lock-mode #'yaml-ts-mode
                  jq-interactive-default-options "--yaml-roundtrip")))
  (advice-add #'jq-interactively :before #'jq-interactively-on-yaml))

(use-package yaml-pro
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(provide 'maj-yaml)
;;; maj-yaml.el ends here
