;;; init-elisp.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs-lisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . eldoc-mode)))

(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . my/paredit-hook))

  :config
  (defun my/paredit-hook ()
    (electric-pair-mode -1)
    (enable-paredit-mode)))

(provide 'init-elisp)
;;; init-elisp.el ends here
