;;; init-elisp.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs-lisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . eldoc-mode)))

(use-package lispy
  :defer t
  :hook ((emacs-lisp-mode . lispy-mode))
  :config
  (unbind-key "M-o" lispy-mode-map)
  (unbind-key "M-S-o" lispy-mode-map))

(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . my/paredit-hook))
  :config
  (defun my/paredit-hook ()
    (electric-pair-mode -1)
    (enable-paredit-mode)))

(provide 'init-elisp)
;;; init-elisp.el ends here
