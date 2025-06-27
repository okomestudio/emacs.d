;;; init.el --- Emacs init configuration  -*- lexical-binding: t -*-
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/emacs.d
;; Keywords: config
;; Package-Requires: ((emacs "30.1"))
;;
;;; Commentary:
;;
;; This feature runs after `early-init.el'.
;;
;;; Code:

(setopt custom-file null-device) ; disable `custom.el'

(use-package dash
  ;; A modern list library
  :hook ((emacs-lisp-mode lisp-data-mode) . dash-fontify-mode)
  :config
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

(use-package ok
  ;; Emacs Lisp utilities for Okome Studio (ok).
  :straight (ok :host github :repo "okomestudio/ok.el")
  :demand t
  :config
  (let ((hook 'hack-dir-local-get-variables-functions))
    (remove-hook hook #'hack-dir-local--get-variables)
    (add-hook hook #'ok-file-hack-dir-local--get-variables))

  (ok-debug-register 'debug-on-error
                     'native-comp-async-report-warnings-errors)
  (with-eval-after-load 'init-loader
    (ok-debug-register 'init-loader-show-log-after-init))
  (with-eval-after-load 'use-package-core
    (ok-debug-register 'use-package-compute-statistics
                       'use-package-verbose))
  (with-eval-after-load 'lsp-mode
    (ok-debug-register 'lsp-log-io)))

(use-package no-littering
  ;; Tidy up config file locations; run this as early as possible.
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

;; Load private initialization.
(load (ok-file-expand-etc "emacs/init") t)

;; Load features under `init.d/'.
(use-package init-loader
  :demand t
  :custom (init-loader-byte-compile nil)
  :config
  (init-loader-load (ok-file-expand-user-emacs-file "init.d")))

(use-package desktop
  ;; Save the Emacs state across sessions.
  :straight nil
  :custom (desktop-auto-save-timeout 180)

  :init
  ;; Loading the feature will set up after-init hook to actually read
  ;; a previously saved session. The session read will be skipped when
  ;; Emacs is launched with `--no-desktop` option.
  (desktop-save-mode 1)

  :config
  (require 'ok-desktop)
  (add-to-list 'desktop-globals-to-save 'safe-local-variable-directories))

(provide 'init)
;;; init.el ends here
