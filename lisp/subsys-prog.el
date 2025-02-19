;;; subsys-prog.el --- Programming Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the programming subsystem.
;;
;;; Code:

;;; Code editing
(use-package lisp
  :straight nil
  :bind (("C-x n d" . narrow-to-defun)
         ("C-x n w" . widen)))

;;; Formatting
(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier"))

;;; Visibility utilities
(use-package treesit-fold
  :straight (treesit-fold :type git
                          :host github
                          :repo "emacs-tree-sitter/treesit-fold"))

(provide 'subsys-prog)
;;; subsys-prog.el ends here
