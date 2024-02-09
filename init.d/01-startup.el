;;; 01-startup.el --- Startup  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :straight nil

  :bind
  (("<f5>" . 'okutil-revert-buffer-no-confirm)
   ("C-S-o" . 'okutil-insert-newline-above)
   ("C-c C-x SPC" . 'okutil-insert-zero-width-space)
   ("C-o" . 'okutil-insert-newline-below)
   ("M-q" . 'okutil-fill-or-unfill-paragraph))

  :custom
  (async-shell-command-buffer "new-buffer")
  (case-fold-search t)
  (compilation-scroll-output t)
  (confirm-kill-processes nil)
  (enable-recursive-minibuffers t)
  (load-prefer-newer t)
  (next-error-message-highlight t)
  (ring-bell-function 'ignore)              ; Disable beeping (in C source code)
  (tab-width 2)
  (uniquify-buffer-name-style 'forward)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (word-wrap-by-category t)

  ;; Basic editing
  (sentence-end-double-space nil)       ; in paragraphs.el
  (show-paren-context-when-offscreen t)
  (show-paren-delay 0)
  (size-indication-mode t)
  (tab-always-indent t)                 ; in indent.el

  ;; File related config
  (auto-save-default nil)
  (backup-by-copying t)
  (backup-directory-alist `((".*" . ,(expand-file-name ".cache/backups"
                                                       user-emacs-directory))))
  (create-lockfiles nil)
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
  (require 'okutil)

  (column-number-mode t)
  (global-so-long-mode +1)              ; mitigate perf on files with long lines
  (show-paren-mode +1)                  ; highlight matching parens
  (subword-mode)
  (tooltip-mode 1)
  (setq-default indent-tabs-mode nil)

  ;; For multilingual environment:
  (set-language-environment "UTF-8") ;; or "Japanese", for example

  ;; avoid prefer-coding-system (see https://github.com/takueof/.emacs.d/blob/master/init.el):
  (set-coding-system-priority 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8))


;; SHELL

(use-package add-node-modules-path
  ;; Add node_modules/.bin to exec-path.
  :defer t
  )


(use-package direnv
  ;; direnv integration.
  ;;
  ;; Invoke direnv to obtain the environment for the current file, then update
  ;; the emacs variables process-environment and exec-path.
  ;;
  :defer t

  :ensure-system-package
  (direnv . "sudo apt install -y direnv")

  :config
  (direnv-mode))


(use-package exec-path-from-shell
  ;; Make Emacs use the PATH set up by the user's shell.
  ;;
  ;; Ensure environment variables look the same in the user's shell.
  ;;
  :defer t
  :if (or (memq window-system '(mac ns x)) (daemonp))

  :config
  (exec-path-from-shell-initialize))


(use-package keychain-environment
  ;; Loads keychain environment variables into emacs.
  :defer t

  :straight
  (:host github :repo "tarsius/keychain-environment")

  :config
  (keychain-refresh-environment))


;; MISC.

(use-package switch-buffer-functions
  ;; Hook run when switching current buffer.
  :disabled)


(use-package system-packages
  :defer t
  :custom
  (system-packages-use-sudo t)
  (system-packages-package-manager 'apt))


(use-package tramp
  :defer t
  :straight nil
  :custom (tramp-default-method "ssh"))

;;; 01-startup.el ends here
