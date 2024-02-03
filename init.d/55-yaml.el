;;; 55-yaml.el --- YAML  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :defer t

  :hook
  (yaml-mode . (lambda () (typo-mode -1)))

  :mode
  ("\\.ya?ml\\'" . yaml-mode))


(use-package lsp-mode
  :defer t

  :hook
  (yaml-mode . (lambda ()
                 ;; If in ansible-mode, do not activate yamlls.
                 (if (not (bound-and-true-p ansible))
                     (init-lsp-lsp-mode-hook 'yamlls)))))

;;; 55-yaml.el ends here
