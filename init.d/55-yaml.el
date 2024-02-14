;;; 55-yaml.el --- YAML  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook
  ((yaml-mode yaml-ts-mode) . (lambda () (typo-mode -1)))

  :init
  (add-to-list 'treesit-language-source-alist
               '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))

  (unless (treesit-language-available-p 'yaml)
    (treesit-install-language-grammar 'yaml))

  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

;;; 55-yaml.el ends here
