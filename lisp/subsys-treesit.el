;;; subsys-treesit.el --- Tree-sitter  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Tree-sitter subsystem.
;;
;;; Code:

(use-package treesit
  :demand t
  :custom ((treesit-auto-install-grammar 'always)
           (treesit-enabled-modes t)
           (treesit-font-lock-level 4))) ; usually 3 is enough

(use-package treesit-fold)

(provide 'subsys-treesit)
;;; subsys-treesit.el ends here
