;;; init-elisp.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs-lisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . eldoc-mode)))

(provide 'init-elisp)
;;; init-elisp.el ends here
