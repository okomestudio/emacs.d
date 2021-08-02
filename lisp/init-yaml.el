;;; init-yaml.el --- Yaml  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :hook
  ((yaml-mode . (lambda () (typo-mode -1)))
   (yaml-mode . remove-trailing-whitespaces-on-save))

  :mode "\\.ya?ml\\'" "\\.ya?ml.j2\\'")

(provide 'init-yaml)
;;; init-yaml.el ends here
