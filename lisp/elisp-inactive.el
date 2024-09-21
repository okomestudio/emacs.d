;;; elisp-inactive.el --- Elisp (Inactive)  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elsa
  :ensure-system-package
  (eask . "curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh")
  :config (elsa-lsp-register))

(use-package flycheck-elsa
  :custom (flycheck-elsa-backend 'eask)
  :hook (emacs-lisp-mode . flycheck-elsa-setup))

(use-package nameless
  ;; See `read-symbol-shorthands' for built-in approach.
  :hook (emacs-lisp-mode . nameless-mode)
  :custom ((nameless-global-aliases '())
           (nameless-private-prefix t)))

;;; elisp-inactive.el ends here
