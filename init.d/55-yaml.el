;;; 55-yaml.el --- YAML  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook
  ((yaml-mode yaml-ts-mode) . (lambda () (typo-mode -1))))

;;; 55-yaml.el ends here
