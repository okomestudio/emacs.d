;;; init-yaml.el --- YAML  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package yaml-mode
  :hook
  (yaml-mode . (lambda () (typo-mode -1)))

  :mode
  ("\\.ya?ml\\'" . yaml-mode))


(use-package lsp-mode
  :hook
  (yaml-mode . (lambda ()
                 ;; If in ansible-mode, do not activate yamlls.
                 (if (not (bound-and-true-p ansible))
                     (init-lsp-lsp-mode-hook 'yamlls)))))


(provide 'init-yaml)
;;; init-yaml.el ends here
