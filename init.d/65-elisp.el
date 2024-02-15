;;; 65-elisp.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Emacs Lisp related utilities.
;;
;;; Code:

(use-package elisp-mode
  :straight nil
  :bind
  (;; no global key binding
   :map emacs-lisp-mode-map
   ("C-c b" . ok-elisp--format-elisp-buffer)
   :map lisp-data-mode-map
   ("C-c b" . ok-elisp--format-elisp-buffer))

  :hook
  (emacs-lisp-mode . (lambda () (require 'company) (company-mode)))
  (emacs-lisp-mode . (lambda () (setq-local devdocs-current-docs '("lisp"))))

  :config
  (defun ok-elisp--format-elisp-buffer ()
    "Format Elisp buffer."
    (interactive)
    (save-excursion
      (indent-region (progn (beginning-of-buffer) (point))
                     (progn (end-of-buffer) (point))))))


(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))


(use-package erefactor)


(use-package eros
  :hook (emacs-lisp-mode . eros-mode))


(use-package flycheck-package
  :hook (emacs-lisp-mode . flycheck-package-setup))


(use-package ipretty
  :config (ipretty-mode 1))


(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))


(use-package paredit
  :hook (emacs-lisp-mode . (lambda ()
                             (electric-pair-local-mode -1)
                             (enable-paredit-mode))))


;; SYNTAX HIGHLIGHTING

(use-package highlight-defined
  :custom
  (highlight-defined-face-use-itself t)

  :hook
  (help-mode . highlight-defined-mode)
  (emacs-lisp-mode . highlight-defined-mode))


(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))


(use-package highlight-sexp
  :hook (emacs-lisp-mode . highlight-sexp-mode)
  :config
  (let* ((mode (frame-parameter nil 'background-mode))
         (scale (if (string= mode "dark") 1.04 0.96))
         (bg (face-attribute 'default :background))
         (bg-hl (okutil-color-scale bg scale)))
    (setq hl-sexp-background-color bg-hl)))


;; ELSA

(use-package elsa
  :disabled
  :ensure-system-package
  (eask . "curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh")

  :config
  (elsa-lsp-register))


(use-package flycheck-elsa
  :disabled
  :custom
  (flycheck-elsa-backend 'eask)

  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))


;; HELP & DOCUMENTATION

(use-package suggest
  ;; For discovering elisp functions based on examples.
  )

;; Local Variables:
;; nameless-aliases: (("" . "ok-elisp"))
;; End:
;;; 65-elisp.el ends here
