;;; init-elisp.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'okutil)


(use-package elsa
  :disabled
  :ensure-system-package
  (eask . "curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh")

  :config
  (elsa-lsp-register))


(use-package erefactor)


(use-package eros
  :hook
  (emacs-lisp-mode . eros-mode))


(use-package flycheck-elsa
  :disabled
  :custom
  (flycheck-elsa-backend 'eask)

  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))


(use-package flycheck-package
  :hook
  (emacs-lisp-mode . flycheck-package-setup))


(use-package ipretty
  :defer t
  :config
  (ipretty-mode 1))


(use-package nameless
  :hook
  (emacs-lisp-mode .  nameless-mode)

  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))


(use-package paredit
  :defer t
  :hook
  (emacs-lisp-mode . init-elisp--paredit-hook)

  :config
  (defun init-elisp--paredit-hook ()
    (electric-pair-mode -1)
    (enable-paredit-mode)))


(use-package suggest
  :defer t)


;; SYNTAX HIGHLIGHTING

(use-package highlight-defined
  :custom
  (highlight-defined-face-use-itself t)

  :hook
  (help-mode . highlight-defined-mode)
  (emacs-lisp-mode . highlight-defined-mode))


(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))


(use-package highlight-sexp
  :custom
  ;; (hl-sexp-background-color "gray95")
  (hl-sexp-background-color (okutil-color-scale '(#xff #xfc #xf9) 0.96))

  :hook
  (emacs-lisp-mode . highlight-sexp-mode))


;; HELP & DOCUMENTATION

(use-package devdocs
  :hook
  (emacs-lisp-mode
   . (lambda () (setq-local devdocs-current-docs '("elisp")))))


(provide 'init-elisp)
;;; init-elisp.el ends here
