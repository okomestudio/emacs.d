;;; init-startup.el --- Startup  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package emacs
  :custom
  (async-shell-command-buffer "new-buffer")
  (case-fold-search t)
  (compilation-scroll-output t)
  (enable-recursive-minibuffers t)
  (load-prefer-newer t)
  (next-error-message-highlight t)
  (read-process-output-max (* 4 1024 1024)) ; 4 mb
  (ring-bell-function 'ignore)              ; Disable beeping (in C source code)
  (tab-width 2)
  (uniquify-buffer-name-style 'forward)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (x-select-request-type
   (cond ((eq window-system 'pgtk) nil)
         ((eq window-system 'x) '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
         (t nil)))
  (word-wrap-by-category t)

  :init
  (setq-default frame-title-format
                '((:eval
                   (list (if (buffer-file-name)
                             (abbreviate-file-name
                              (expand-file-name buffer-file-name))
                           (buffer-name))))
                  " - Emacs"))

  (when window-system
    (setq select-enable-clipboard t)))


(use-package emacs ;; scroll configuration
  :custom
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)
                                 ((meta))
                                 ((control meta) . global-text-scale)
                                 ((control) . text-scale)))
  (pixel-scroll-precision-interpolation-factor 1.2)
  (pixel-scroll-precision-large-scroll-height 1.0)
  (pixel-scroll-precision-momentum-min-velocity 0.5)
  (pixel-scroll-precision-momentum-seconds 0.5) ; 1.75
  (pixel-scroll-precision-momentum-tick 0.05)
  (pixel-scroll-precision-interpolation-total-time 0.1)
  (pixel-scroll-precision-interpolation-between-scroll 0.001)

  :init
  (pixel-scroll-precision-mode +1))


(use-package simple
  ;; Basic editing commands for Emacs.
  :straight nil

  :bind
  (("<f5>" . 'okutil-revert-buffer-no-confirm)
   ("C-S-o" . 'okutil-insert-newline-above)
   ("C-c C-x SPC" . 'okutil-insert-zero-width-space)
   ("C-o" . 'okutil-insert-newline-below)
   ("M-q" . 'okutil-fill-or-unfill-paragraph))

  :custom
  (save-interprogram-paste-before-kill t)
  (sentence-end-double-space nil)       ; in paragraphs.el
  (show-paren-context-when-offscreen t)
  (show-paren-delay 0)
  (size-indication-mode t)
  (tab-always-indent t)                 ; in indent.el

  :init
  (require 'okutil)

  (column-number-mode t)
  (global-so-long-mode +1)              ; mitigate perf on files with long lines
  (show-paren-mode +1)                  ; highlight matching parens
  (subword-mode)
  (tooltip-mode 1)
  (setq-default indent-tabs-mode nil)

  ;; Global minor mode to undo (C-c <left>) or redo (C-c <right>) a change in
  ;; window configuration.
  (winner-mode 1)

  ;; For multilingual environment:
  (set-language-environment "UTF-8") ;; or "Japanese", for example

  ;; avoid prefer-coding-system (see https://github.com/takueof/.emacs.d/blob/master/init.el):
  (set-coding-system-priority 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8))


(use-package files
  :straight nil

  :custom
  (auto-save-default nil)
  ;; (auto-save-file-name-transforms '((".*" ts/backup-cache-dir t)))
  (backup-by-copying t)
  ;; (backup-directory-alist '(("." . ts/backup-cache-dir)))
  (backup-directory-alist `((".*" . ,ts/backup-cache-dir)))
  (create-lockfiles nil)
  (make-backup-files nil)
  (require-final-newline nil)

  :preface
  (defconst ts/backup-cache-dir (expand-file-name "~/.cache/emacs-backups"))

  :init
  (require 'okutil)
  (okutil-ensure-directory-exists ts/backup-cache-dir))


(use-package browse-url
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (browse-url-generic-program "xdg-open")
  (browse-url-handlers '(("localhost" . browse-url-generic))))


;; WINDOWS AND FRAMES

(use-package ace-window
  ;; Quickly switch windows in Emacs.
  :bind
  (("M-O" . 'ace-window))

  :custom
  (aw-dispatch-always t))


(use-package frame-cmds
  ;; Frame functions and commands.
  :bind
  ("M-o" . 'other-window-or-frame))


(use-package yascroll
  ;; Yet Another Scroll Bar Mode.
  :init
  (global-yascroll-bar-mode +1))


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
  :ensure-system-package
  (direnv . "sudo apt install -y direnv")

  :config
  (direnv-mode))


(use-package exec-path-from-shell
  ;; Make Emacs use the PATH set up by the user's shell.
  ;;
  ;; Ensure environment variables look the same in the user's shell.
  ;;
  :if (or (memq window-system '(mac ns x)) (daemonp))

  :config
  (exec-path-from-shell-initialize))


(use-package keychain-environment
  ;; Loads keychain environment variables into emacs.
  :straight
  (:host github :repo "tarsius/keychain-environment")

  :config
  (keychain-refresh-environment))


;; MISC.

(use-package dash
  ;; A modern list library for Emacs
  )


(use-package tramp
  :straight nil
  :custom (tramp-default-method "ssh")
  :defer t)


(use-package uuid)


(use-package system-packages
  :custom
  (system-packages-use-sudo t)
  (system-packages-package-manager 'apt))


(use-package switch-buffer-functions
  ;; Hook run when switching current buffer.
  )


(provide 'init-startup)
;;; init-startup.el ends here
