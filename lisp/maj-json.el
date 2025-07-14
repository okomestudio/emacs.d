;;; maj-json.el --- JSON Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the JSON major mode.
;;
;; NOTE(2025-02-13): `json-ts-mode' complains that tree-sitter library
;; is missing. It doesn't automatically download, so you may need to
;; build and install manually (github.com/casouri/tree-sitter-module).
;;
;;; Code:

(use-package json-ts-mode
  :mode "\\.json\\(\\.j2\\)?\\'"
  :bind ( :map json-ts-mode-map
          ("C-c b" . json-ts-mode-format-code) )
  :hook (json-ts-mode . lsp-deferred)
  :custom (json-ts-mode-indent-offset 2)
  :config
  (defun json-ts-mode-format-code ()
    (interactive)
    (prettier-js)))

(use-package jq-mode
  :after json-ts-mode
  :bind ( :map json-ts-mode-map
          ("C-c C-j" . jq-interactively) )
  :ensure-system-package (jq . "sudo apt install -y jq")
  :config
  (defun jq-interactively-on-json (&rest r)
    (when (derived-mode-p 'json-ts-mode)
      (setq-local jq-interactive-command "jq"
                  jq-interactive-font-lock-mode #'json-ts-mode
                  jq-interactive-default-options "")))
  (advice-add #'jq-interactively :before #'jq-interactively-on-json))

(provide 'maj-json)
;;; maj-json.el ends here
