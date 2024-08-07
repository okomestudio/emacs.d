;;; 00-startup.el --- Startup  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Emacs startup settings.
;;
;;; Code:

(use-package ok
  :demand t
  :straight (:host github :repo "okomestudio/ok.el"))


(use-package emacs
  :demand t
  :straight nil
  :bind (("<f5>" . 'ok-buffer-revert-no-confirm)
         ("C-S-o" . 'ok-edit-insert-newline-above)
         ("C-c C-x SPC" . 'ok-edit-insert-zero-width-space)
         ("C-o" . 'ok-edit-insert-newline-below)
         ("M-q" . 'ok-edit-fill-or-unfill-paragraph))

  :custom
  (async-shell-command-buffer "new-buffer")
  (case-fold-search t)
  (compilation-scroll-output t)
  (confirm-kill-processes nil)
  (enable-recursive-minibuffers t)
  (initial-major-mode #'lisp-interaction-mode) ;; for `*scratch*' buffer
  (load-prefer-newer t)
  (next-error-message-highlight t)
  (tab-width 2)
  (uniquify-buffer-name-style 'forward)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (word-wrap-by-category t)

  ;; Basic editing
  (sentence-end-double-space nil) ;; in paragraphs.el
  (show-paren-context-when-offscreen t)
  (show-paren-delay 0)
  (size-indication-mode t)

  ;; File related config
  (auto-save-default nil)
  (backup-by-copying t)
  (create-lockfiles nil)
  (history-length 25)
  (make-backup-files nil)
  (require-final-newline nil)

  ;; Frame
  (frame-title-format '((:eval
                         (list (if (buffer-file-name)
                                   (abbreviate-file-name
                                    (expand-file-name buffer-file-name))
                                 (buffer-name))))
                        " - Emacs"))

  :init
  (column-number-mode t)
  (global-so-long-mode +1) ;; mitigate perf on files with long lines
  (show-paren-mode +1)     ;; highlight matching parens
  (subword-mode)
  (tooltip-mode 1)
  (setq-default indent-tabs-mode nil)

  ;; For multilingual environment:
  (set-language-environment "UTF-8") ;; or "Japanese", for example

  ;; avoid prefer-coding-system (see https://github.com/takueof/.emacs.d/blob/master/init.el):
  (set-coding-system-priority 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8))

(use-package autorevert
  :straight nil
  :custom
  (auto-revert-avoid-polling t)
  ;; (global-auto-revert-non-file-buffers t)
  :init (global-auto-revert-mode 1))

(use-package indent
  :straight nil
  :custom ((tab-always-indent 'complete)))


;; COMMON ELISP LIBRARIES

(use-package dash) ;; A modern list library for Emacs
(use-package uuid)


;; SHELL

(use-package add-node-modules-path
  ;; Add node_modules/.bin to exec-path.
  )


(use-package direnv
  ;; direnv integration.
  ;;
  ;; Invoke direnv to obtain the environment for the current file, then update
  ;; the emacs variables process-environment and exec-path.
  ;;
  :ensure-system-package (direnv . "sudo apt install -y direnv")
  :config (direnv-mode))


(use-package exec-path-from-shell
  ;; Make Emacs use the PATH set up by the user's shell.
  ;;
  ;; Ensure environment variables look the same in the user's shell.
  ;;
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :init (exec-path-from-shell-initialize))


(use-package keychain-environment
  ;; Loads keychain environment variables into emacs.
  :straight (:host github :repo "tarsius/keychain-environment")
  :config (keychain-refresh-environment))


;; MISC.

(use-package anzu
  ;; Displays current and total matches information in the mode-line.
  :hook (after-init . (lambda () (global-anzu-mode 1))))


(use-package switch-buffer-functions
  ;; Hook run when switching current buffer.
  :disabled)


(use-package system-packages
  :custom
  (system-packages-use-sudo t)
  (system-packages-package-manager 'apt))


(use-package tramp
  :straight nil
  :custom (tramp-default-method "ssh"))


;; TREE-SITTER

(use-package treesit
  :demand t
  :straight nil)


(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; 00-startup.el ends here
