;;; elisp-inactive.el --- Elisp (Inactive)  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nameless
  ;; See `read-symbol-shorthands' for built-in approach.
  :hook (emacs-lisp-mode . nameless-mode)
  :custom ((nameless-global-aliases '())
           (nameless-private-prefix t)))

;;; elisp-inactive.el ends here
