;;; init.el --- Emacs init  -*- lexical-binding: t -*-
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/emacs.d
;; Keywords: config
;; Package-Requires: ((emacs "30.1"))
;;
;;; Commentary:
;;
;; This runs after `early-init.el'.
;;
;;; Code:

;;; Emacs User Profile

(defvar emacs-user-profile "default"
  "Emacs user profile.
Used to customize the `init-loader' and `desktop' save location.")

(when-let* ((profile (getenv "EMACS_USER_PROFILE"))
            (profile (when (< 0 (length profile))
                       profile)))
  (setq-default emacs-user-profile profile))

;;; Early Customization

(setopt custom-file null-device) ; disable `custom.el'

;; Make the compilation buffer comint by default:
(advice-add #'compile :filter-args
            (lambda (command &optional _comint) `(,command t)))

;;; External Dependencies
;;
;; Install packages used throughout the init files here.

(use-package ok
  ;; Emacs Lisp utilities for Okome Studio (ok).
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
  ;; Redirect package config files to etc/ or var/.
  :demand t
  :custom ((no-littering-etc-directory (locate-user-emacs-file "etc/"))
           (no-littering-var-directory (locate-user-emacs-file "var/")))
  :config
  ;; For filesystem access with shorter function names:
  (defun fs-emacs-etc (&rest parts)
    (no-littering-expand-etc-file-name (apply #'file-name-concat parts)))

  (defun fs-emacs-var (&rest parts)
    (no-littering-expand-var-file-name (apply #'file-name-concat parts)))

  (defun fs-emacs (&rest parts)
    (locate-user-emacs-file (apply #'file-name-concat parts)))

  (defun fs-emacs-bin (&rest parts)
    "Expand the path to FILE in Emacs's 'bin/' directory."
    (fs-emacs (apply #'file-name-concat `("bin" ,@parts))))

  (defun fs-emacs-lisp (&rest parts)
    "Expand the path to FILE in Emacs's 'lisp/' directory."
    (fs-emacs (apply #'file-name-concat `("lisp" ,@parts))))

  (defun fs-straight-repo (&rest parts)
    "Expand the path to FILE in Emacs's 'straight/repos/' directory."
    (fs-emacs (apply #'file-name-concat `("straight" "repos" ,@parts)))))

;;; Private Initialization

(load (fs-emacs-etc "emacs/init") t)

;;; Package Loading from `init.d/'.

(use-package init-loader
  :demand t
  :custom ((init-loader-byte-compile nil)
           (init-loader-directory (fs-emacs "init.d" emacs-user-profile)))
  :config
  (unless (file-directory-p init-loader-directory)
    (error "No init-loader directory exists (%s)" init-loader-directory))
  (init-loader-load))

;;; Session Persistence

(use-package desktop
  ;; Save the Emacs state across sessions.
  :custom ((desktop-auto-save-timeout 180)
           (desktop-modes-not-to-save '(eww-mode tags-table-mode)))
  :init
  (let ((dir (directory-file-name (fs-emacs-var "desktop" emacs-user-profile))))
    (make-directory desktop-dirname t)
    (setq-default desktop-dirname dir)
    (setopt desktop-path (list desktop-dirname)))

  ;; Loading the feature will set up after-init hook to actually read
  ;; a previously saved session. The session read will be skipped when
  ;; Emacs is launched with `--no-desktop` option.
  (desktop-save-mode 1)

  :config
  (require 'ok-desktop)       ; ensures application of enhancements

  ;; Use if any globals should be saved.
  (add-to-list 'desktop-globals-to-save 'safe-local-variable-directories))

(provide 'init)
;;; init.el ends here
