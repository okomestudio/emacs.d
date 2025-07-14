;;; maj-elisp.el --- Emacs Lisp Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Emacs Lisp major mode.
;;
;;; Code:

(require 'ok)

(use-package elisp-mode
  :bind ( :map emacs-lisp-mode-map
          ("C-c b" . elisp-mode-ok--format)
          :map lisp-data-mode-map
          ("C-c b" . elisp-mode-ok--format) )
  :hook (((emacs-lisp-mode lisp-data-mode) . elisp-mode-ok--style)
         ((emacs-lisp-mode lisp-data-mode) . elisp-mode-ok--capf-set))
  :config
  (defun elisp-mode-ok--format ()
    "Format the current Emacs Lisp buffer."
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max))))

  (defun elisp-mode-ok--style ()
    "Enforce the coding style in the Emacs Lisp buffer."
    (setq-local emacs-lisp-docstring-fill-column 80
                fill-column 80
                indent-tabs-mode nil
                tab-width 2))

  (defun elisp-mode-ok--capf (&rest args)
    (apply (cape-capf-inside-code #'cape-elisp-symbol) args))

  (defun elisp-mode-ok--capf-set ()
    "Set CAPFs for the Emacs Lisp mode."
    ;; See github.com/jwiegley/use-package/issues/1077#issuecomment-2266642373
    (add-hook 'completion-at-point-functions #'elisp-mode-ok--capf -99 t)))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-data-mode) . aggressive-indent-mode))

(use-package elp
  ;; Emacs Lisp Profiler.
  ;;
  ;; Run and specify the prefix of module name to profile
  ;;
  ;;   M-x elp-instrument-function
  ;;
  ;; Then, to show results, run
  ;;
  ;;   M-x elp-results
  ;;
  :custom ((elp-sort-by-function 'elp-sort-by-average-time)))

(use-package erefactor)

(use-package eros
  :hook ((emacs-lisp-mode lisp-data-mode) . eros-mode))

(use-package flycheck-package
  :hook ((emacs-lisp-mode lisp-data-mode) . flycheck-package-setup))

(use-package ipretty
  ;; Provides interactive functions to pretty-print the result of an expression.
  :bind ( :map emacs-lisp-mode-map
          ("C-x C-S-e" . ipretty-ok-last-sexp)
          :map lisp-interaction-mode-map
          ("C-x C-S-e" . ipretty-ok-last-sexp)
          :map lisp-data-mode-map
          ("C-x C-S-e" . ipretty-ok-last-sexp) )
  :config
  (defun ipretty-ok-last-sexp (arg)
    (interactive "P")
    (pcase arg
      ('(4) (ipretty-last-sexp-other-buffer))
      (_ (ipretty-last-sexp)))))

(use-package macrostep
  ;; Interactive macro-expander.
  )

(use-package paredit
  ;; Parentheses editing.
  :hook ((emacs-lisp-mode lisp-data-mode) . paredit-ok--enable-paredit)
  :config
  (setq paredit-comment-prefix-margin "; ")

  (defun paredit-ok--enable-paredit ()
    (setq-local comment-column 30)

    ;; Turn off `electric-pair-mode' to avoid conflict with `paredit-mode':
    (electric-pair-local-mode -1)
    (enable-paredit-mode)))

;; Syntax Highlighting

(use-package highlight-defined
  :custom (highlight-defined-face-use-itself t)
  :hook ((help-mode . highlight-defined-mode)
         ((emacs-lisp-mode lisp-data-mode) . highlight-defined-mode)))

(use-package highlight-quoted
  :hook ((emacs-lisp-mode lisp-data-mode) . highlight-quoted-mode))

(use-package highlight-sexp
  ;; Highlight the current zone according to its context (sexp, comment, string,
  ;; etc.).
  :hook (((emacs-lisp-mode lisp-data-mode) . highlight-sexp-mode)
         (enable-theme-functions . highlight-sexp-ok--refresh))
  :config
  (defun highlight-sexp-ok--refresh (theme)
    "Refresh "
    (setopt hl-sexp-background-color
            (ok-face-color-scale (face-attribute 'default :background)
                                 (pcase (frame-parameter nil 'background-mode)
                                   ('dark 1.04)
                                   ('light 0.96)
                                   (_ 1.00))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p highlight-sexp-mode)
          (highlight-sexp-mode -1)
          (highlight-sexp-mode 1))))))

;; Help & Documentation

(use-package package-lint)

(use-package suggest
  ;; For discovering elisp functions based on examples.
  )

(provide 'maj-elisp)
;;; maj-elisp.el ends here
