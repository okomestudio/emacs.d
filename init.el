;;; init.el --- Emacs startup configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This runs after `early-init.el'.
;;
;;; Code:

;; Disable `custom.el'
(setopt custom-file null-device)

(use-package ok
  ;; Elisp utilities for Okome Studio. Loaded early so that they can
  ;; be used in `init.el'
  :straight (ok :host github :repo "okomestudio/ok.el")
  :demand t)

(use-package no-littering
  ;; Run this as early as possible.
  :demand t
  :custom ((no-littering-etc-directory (ok-file-expand-user-emacs-file "etc/"))
           (no-littering-var-directory (ok-file-expand-user-emacs-file "var/")))
  :config
  (defun ok-file-expand-bin (&rest components)
    "Expand the path to FILE in Emacs's bin/ directory."
    (apply #'ok-file-expand-user-emacs-file `("bin" ,@components)))

  (defun ok-file-expand-lisp (&rest components)
    "Expand the path to FILE in Emacs's lisp/ directory."
    (apply #'ok-file-expand-user-emacs-file `("lisp" ,@components)))

  (defun ok-file-expand-straight-repos (&rest components)
    "Expand the path to FILE in Emacs's straight/repos directory."
    (apply #'ok-file-expand-user-emacs-file `("straight" "repos" ,@components)))

  ;; Define aliases for shorter names.
  (defalias 'ok-file-expand-etc #'no-littering-expand-etc-file-name)
  (defalias 'ok-file-expand-var #'no-littering-expand-var-file-name))

;; Load config files under `init.d/'
(use-package init-loader
  :demand t
  :custom ((init-loader-byte-compile nil)
           (init-loader-show-log-after-init ok-debug))
  :config (init-loader-load (ok-file-expand-user-emacs-file "init.d")))

;;; init.el ends here
