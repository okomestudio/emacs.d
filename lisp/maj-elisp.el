;;; maj-elisp.el --- Emacs Lisp Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Emacs Lisp major mode.
;;
;;; Code:

(require 'ok)

(use-package elisp-mode
  :bind ( :map emacs-lisp-mode-map
          ("C-c b" . elisp-mode--format)
          :map lisp-data-mode-map
          ("C-c b" . elisp-mode--format) )
  :custom (emacs-lisp-docstring-fill-column 72)
  :config
  (defun elisp-mode--format ()
    "Format the current Emacs Lisp buffer."
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max))))

  (defun elisp-mode--init ()
    "Enforce the coding style in the Emacs Lisp buffer."
    (setq-local fill-column 72
                comment-fill-column 72
                tab-width 8)
    (indent-tabs-mode -1)

    (setq-local display-fill-column-indicator-column 80)
    (set-face-attribute 'fill-column-indicator nil
                        :foreground (face-attribute 'hl-line :background))
    (display-fill-column-indicator-mode 1))

  (defun elisp-mode--capf (&rest args)
    (apply (cape-capf-inside-code #'cape-elisp-symbol) args))

  (defun elisp-mode--capf-set ()
    "Set CAPFs for the Emacs Lisp mode."
    ;; See github.com/jwiegley/use-package/issues/1077#issuecomment-2266642373
    (add-hook 'completion-at-point-functions #'elisp-mode--capf -99 t))

  ;; Fontify `dash' symbols when the package is loaded.
  (with-eval-after-load 'dash
    (add-hook 'emacs-lisp-mode-hook #'dash-fontify-mode)
    (add-hook 'lisp-data-mode-hook #'dash-fontify-mode))

  :hook (((emacs-lisp-mode lisp-data-mode) . elisp-mode--init)
         ((emacs-lisp-mode lisp-data-mode) . elisp-mode--capf-set)))

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

(use-package nameless
  ;; Hide package namespace in Emacs Lisp code.
  ;;
  ;; See `read-symbol-shorthands' for a built-in approach.
  :custom ((nameless-affect-indentation-and-filling nil)
           (nameless-global-aliases nil)
           (nameless-private-prefix t))
  :config
  ;; Hot-fix indentation before saving.
  ;;
  ;; `nameless-mode' does not fix indentation back from the visible length
  ;; before saving. The following fix temporarily deactivates `nameless-mode',
  ;; fixes indentation, saves the buffer, and then recover `nameless-mode'.
  ;;
  ;; See https://github.com/Malabarba/Nameless/issues/18.
  (defun nameless--before-save ()
    "Remove keywords and reindent buffer before saving."
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (bound-and-true-p nameless-mode))
      (let ((font-lock-enabled font-lock-mode)
            (aggressive-indent-enabled aggressive-indent-mode))
        (unwind-protect
            (progn
              (when aggressive-indent-enabled (aggressive-indent-mode -1))
              (when font-lock-enabled (font-lock-mode -1))
              (nameless--remove-keywords)
              (save-restriction
                (widen)
                (indent-region (point-min) (point-max))))
          (when font-lock-enabled (font-lock-mode 1))
          (when aggressive-indent-enabled (aggressive-indent-mode 1))))))

  (defun nameless--after-save ()
    "Replace `nameless-mode' keywords after saving to restore abbreviations."
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (bound-and-true-p nameless-mode))
      ;; TODO(2025-07-21): This does not fully revert the change made by
      ;; `nameless--before-save'.
      (let ((font-lock-enabled font-lock-mode)
            (aggressive-indent-enabled aggressive-indent-mode))
        (unwind-protect
            (progn
              (unless aggressive-indent-enabled (aggressive-indent-mode 1))
              (unless font-lock-enabled (font-lock-mode 1))
              (nameless--after-hack-local-variables)
              (when font-lock-enabled (font-lock-mode -1))
              (save-restriction
                (widen)
                (indent-region (point-min) (point-max))))
          (when font-lock-enabled (font-lock-mode 1))
          (when aggressive-indent-enabled (aggressive-indent-mode 1))
          (set-buffer-modified-p nil)))))

  (defun nameless-mode--setup-or-teardown ()
    "Set up or tear down `nameless-mode'."
    (pcase nameless-mode
      ('t
       (add-hook 'before-save-hook #'nameless--before-save 98 t)
       (add-hook 'after-save-hook #'nameless--after-save -98 t))
      (_
       (remove-hook 'after-save-hook #'nameless--after-save t)
       (remove-hook 'before-save-hook #'nameless--before-save t))))

  :hook ((emacs-lisp-mode . nameless-mode)
         (nameless-mode . nameless-mode--setup-or-teardown)))

(use-package paredit
  ;; Parentheses editing.
  :config
  (setq paredit-comment-prefix-margin "; ")

  (defun paredit--init ()
    (setq-local comment-column 30)

    ;; Turn off `electric-pair-mode' to avoid conflict:
    (electric-pair-local-mode -1)
    (enable-paredit-mode))

  :hook ((emacs-lisp-mode lisp-data-mode) . paredit--init))

;;; Syntax Highlighting

(use-package highlight-defined
  :custom (highlight-defined-face-use-itself t)
  :hook ((help-mode . highlight-defined-mode)
         ((emacs-lisp-mode lisp-data-mode) . highlight-defined-mode)))

(use-package highlight-quoted
  :hook ((emacs-lisp-mode lisp-data-mode) . highlight-quoted-mode))

(use-package highlight-sexp
  ;; Highlight the current zone according to its context (sexp, comment, string,
  ;; etc.).
  :config
  (defun highlight-sexp--on-enable-theme (theme)
    "Refresh `highlight-sexp-mode'."
    (setopt hl-sexp-background-color
            (ok-face-color-scale
	     (face-attribute 'default :background)
             (pcase (frame-parameter nil 'background-mode)
               ('dark 1.04)
               ('light 0.96)
               (_ 1.00))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p highlight-sexp-mode)
          (highlight-sexp-mode -1)
          (highlight-sexp-mode 1)))))

  :hook (((emacs-lisp-mode lisp-data-mode) . highlight-sexp-mode)
         (enable-theme-functions . highlight-sexp--on-enable-theme)))

;;; Help & Documentation

(use-package package-lint)

(use-package suggest
  ;; For discovering elisp functions based on examples.
  )

(provide 'maj-elisp)
;;; maj-elisp.el ends here
