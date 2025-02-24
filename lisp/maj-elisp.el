;;; maj-elisp.el --- Elisp Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Emacs Lisp major mode.
;;
;;; Code:

(require 'ok)

(use-package elisp-mode
  :straight nil
  :bind (:map
         emacs-lisp-mode-map
         ("C-c b" . elisp-mode-ok--format)
         :map lisp-data-mode-map
         ("C-c b" . elisp-mode-ok--format))
  :hook ((emacs-lisp-mode
          lisp-data-mode) . elisp-mode-ok--set-completion-functions)
  :config
  (defun elisp-mode-ok--format ()
    "Format Elisp buffer."
    (interactive)
    (save-excursion
      (indent-region (progn (beginning-of-buffer) (point))
                     (progn (end-of-buffer) (point)))))

  (defun elisp-mode-ok--set-completion-functions ()
    ;; See github.com/jwiegley/use-package/issues/1077#issuecomment-2266642373
    (setq-local completion-at-point-functions
                (list (cape-capf-inside-code #'cape-elisp-symbol)))))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode
          lisp-data-mode) . aggressive-indent-mode))

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
  :straight nil
  :custom ((elp-sort-by-function 'elp-sort-by-average-time)))

(use-package erefactor)

(use-package eros
  :hook ((emacs-lisp-mode
          lisp-data-mode) . eros-mode))

(use-package flycheck-package
  :hook ((emacs-lisp-mode
          lisp-data-mode) . flycheck-package-setup))

(use-package ipretty
  ;; Provides interactive functions to pretty-print the result of an
  ;; expression
  :config (ipretty-mode 1))

(use-package macrostep
  ;; Interactive macro-expander.
  )

(use-package paredit
  :hook ((emacs-lisp-mode
          lisp-data-mode) . paredit-ok--enable-paredit)
  :config
  (defun paredit-ok--enable-paredit ()
    (electric-pair-local-mode -1)
    (enable-paredit-mode)))

;; SYNTAX HIGHLIGHTING

(use-package highlight-defined
  :custom (highlight-defined-face-use-itself t)
  :hook ((help-mode . highlight-defined-mode)
         ((emacs-lisp-mode
           lisp-data-mode) . highlight-defined-mode)))

(use-package highlight-quoted
  :hook ((emacs-lisp-mode
          lisp-data-mode) . highlight-quoted-mode))

(use-package highlight-sexp
  :hook ((emacs-lisp-mode
          lisp-data-mode) . highlight-sexp-mode)
  :config
  (let* ((mode (frame-parameter nil 'background-mode))
         (scale (if (string= mode "dark") 1.04 0.96))
         (bg (face-attribute 'default :background))
         (bg-hl (ok-face-color-scale bg scale)))
    (setopt hl-sexp-background-color bg-hl)))

;; HELP & DOCUMENTATION

(use-package package-lint)

(use-package suggest
  ;; For discovering elisp functions based on examples.
  )

(provide 'maj-elisp)
;;; maj-elisp.el ends here
