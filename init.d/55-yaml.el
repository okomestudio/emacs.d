;;; 55-yaml.el --- yaml  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure YAML and related utilities.
;;
;;; Code:

(use-package yaml-ts-mode
  :straight nil
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-ts-mode . ok-yaml--patch-yaml-ts-mode)
  (yaml-ts-mode . lsp-deferred)

  :config
  (defun ok-yaml--patch-yaml-ts-mode ()
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
    (setq font-lock-defaults '(yaml-font-lock-keywords))))

;;; 55-yaml.el ends here
