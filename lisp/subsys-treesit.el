;;; subsys-treesit.el --- Tree-sitter Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Tree-sitter subsystem.
;;
;;; Code:

(use-package treesit
  :custom (treesit-font-lock-level 4) ; usually 3 is enough
  :demand t)

(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold)

(provide 'subsys-treesit)
;;; subsys-treesit.el ends here
