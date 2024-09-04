;;; 65-elisp.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs Lisp initialization.
;;
;;; Code:

(use-package elisp-mode
  :straight nil
  :bind (; No global key binding
         :map emacs-lisp-mode-map
         ("C-c b" . oe--format-elisp-buffer)
         :map lisp-data-mode-map
         ("C-c b" . oe--format-elisp-buffer))
  :hook ((emacs-lisp-mode lisp-data-mode) . oe--set-completion-functions)
  :config
  (defun oe--format-elisp-buffer ()
    "Format Elisp buffer."
    (interactive)
    (save-excursion
      (indent-region (progn (beginning-of-buffer) (point))
                     (progn (end-of-buffer) (point)))))

  (defun oe--set-completion-functions ()
    ;; See github.com/jwiegley/use-package/issues/1077#issuecomment-2266642373
    (setq-local completion-at-point-functions
                (list (cape-capf-inside-code #'cape-elisp-symbol)))))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-data-mode) . aggressive-indent-mode))

(use-package erefactor)

(use-package eros
  :hook ((emacs-lisp-mode lisp-data-mode) . eros-mode))

(use-package flycheck-package
  :hook ((emacs-lisp-mode lisp-data-mode) . flycheck-package-setup))

(use-package ipretty
  ;; Provides interactive functions to pretty-print the result of an
  ;; expression
  :config (ipretty-mode 1))

(use-package macrostep
  ;; Interactive macro-expander.
  )

(use-package nameless
  ;; See `read-symbol-shorthands' for another, built-in approach.
  :hook (emacs-lisp-mode . nameless-mode)
  :custom ((nameless-global-aliases '())
           (nameless-private-prefix t)))

(use-package paredit
  :hook ((emacs-lisp-mode lisp-data-mode) . oe--enable-paredit)
  :config
  (defun oe--enable-paredit ()
    (electric-pair-local-mode -1)
    (enable-paredit-mode)))

;; SYNTAX HIGHLIGHTING

(use-package highlight-defined
  :custom (highlight-defined-face-use-itself t)
  :hook ((help-mode . highlight-defined-mode)
         ((emacs-lisp-mode lisp-data-mode) . highlight-defined-mode)))

(use-package highlight-quoted
  :hook ((emacs-lisp-mode lisp-data-mode) . highlight-quoted-mode))

(use-package highlight-sexp
  :hook ((emacs-lisp-mode lisp-data-mode) . highlight-sexp-mode)
  :config
  (let* ((mode (frame-parameter nil 'background-mode))
         (scale (if (string= mode "dark") 1.04 0.96))
         (bg (face-attribute 'default :background))
         (bg-hl (ok-face-color-scale bg scale)))
    (setopt hl-sexp-background-color bg-hl)))

;; ELSA

(use-package elsa
  :disabled
  :ensure-system-package
  (eask . "curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh")
  :config (elsa-lsp-register))

(use-package flycheck-elsa
  :disabled
  :custom (flycheck-elsa-backend 'eask)
  :hook (emacs-lisp-mode . flycheck-elsa-setup))

;; HELP & DOCUMENTATION

(use-package package-lint)

(use-package suggest
  ;; For discovering elisp functions based on examples.
  )

;; Local Variables:
;; read-symbol-shorthands: (("oe" . "ok-elisp"))
;; End:
;;; 65-elisp.el ends here
