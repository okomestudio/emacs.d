;;; subsys-prog.el --- Programming Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the programming subsystem.
;;
;;; Code:

;;; Formatting

(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier"))

(use-package treesit-fold
  :straight (treesit-fold :type git
                          :host github
                          :repo "emacs-tree-sitter/treesit-fold"))

(provide 'subsys-prog)
;;; subsys-prog.el ends here
