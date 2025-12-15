;;; subsys-treesit.el --- Tree-sitter Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Tree-sitter subsystem.
;;
;;; Code:

(use-package treesit
  :custom (treesit-font-lock-level 4) ; usually 3 is enough
  :demand t
  :ensure-system-package (tree-sitter . "npm install -g tree-sitter-cli")
  :config
  ;; NOTE(2025-12-15): To avoid 'Warning (treesit): Cannot activate tree-sitter,
  ;; because language grammar for javascript is unavailable (version-mismatch):
  ;; 15'. Manually run `treesit-install-language-grammar' for JavaScript.
  (setq treesit-language-source-alist
        '((javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                      "v0.23.1" "src"))))

(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold)

(provide 'subsys-treesit)
;;; subsys-treesit.el ends here
