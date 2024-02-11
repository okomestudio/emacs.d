;;; 55-yaml.el --- YAML  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :hook
  (yaml-mode . (lambda () (typo-mode -1)))

  :mode
  ("\\.ya?ml\\'" . yaml-mode))

;;; 55-yaml.el ends here
