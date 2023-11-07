;;; init-startup.el --- Startup  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package emacs
  :custom
  (async-shell-command-buffer "new-buffer")
  (case-fold-search t)
  (compilation-scroll-output t)
  (enable-recursive-minibuffers t)
  (inhibit-splash-screen nil)
  (inhibit-startup-screen nil)
  (load-prefer-newer t)
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (next-error-message-highlight t)
  (pixel-scroll-precision-large-scroll-height 5.0)
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
    (setq select-enable-clipboard t))

  (pixel-scroll-precision-mode -1))


(use-package simple
  ;; Basic editing commands for Emacs.
  :straight nil

  :bind
  (("<f5>" . 'ts/revert-buffer-no-confirm)
   ("C-S-o" . 'ts/newline-above)
   ("C-c C-x SPC" . 'ts/insert-zero-width-space)
   ("C-o" . 'ts/newline-below)
   ("M-Q" . 'ts/unfill-paragraph))

  :custom
  (save-interprogram-paste-before-kill t)
  (sentence-end-double-space nil)       ; in paragraphs.el
  (show-paren-context-when-offscreen t)
  (show-paren-delay 0)
  (size-indication-mode t)
  (tab-always-indent t)                 ; in indent.el

  :preface
  (defun ts/insert-zero-width-space ()
    (interactive)
    (insert-char #x200b))

  (defun ts/newline-above ()
    (interactive)
    (back-to-indentation)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode))

  (defun ts/newline-below ()
    (interactive)
    (end-of-line)
    (newline-and-indent))

  (defun ts/revert-buffer-no-confirm (&optional force-reverting)
    "Interactive call to 'revert-buffer'.

    Ignoring the auto-save file and not requesting for confirmation.
    When the current buffer is modified, the command refuses to
    revert it, unless you specify the optional argument:
    FORCE-REVERTING to true."
    (interactive "P")
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified")))

  (defun ts/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  :init
  (column-number-mode t)
  (global-so-long-mode +1)              ; mitigate perf on files with long lines
  (show-paren-mode +1)                  ; highlight matching parens
  (subword-mode)
  (tooltip-mode 1)
  (setq-default indent-tabs-mode nil)

  ;; Global minor mode to undo (C-c <left>) or redo (C-c <right>) a change in
  ;; window configuration.
  (winner-mode 1)

  ;; For multilingual environment.
  (prefer-coding-system 'utf-8)         ; Use UTF-8 when possible
  (set-default-coding-systems 'utf-8)
  (set-language-environment "UTF-8"))


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
  (ensure-directory-exists ts/backup-cache-dir))


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


(use-package shackle
  ;; Enforce rules for popup windows.
  :disabled

  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 0.4)
  (shackle-rules '(("*Warnings*"
                    :select nil :size 0.25)
                   (magit-status-mode
                    :align right :size 0.5 :inhibit-window-quit t :other t)))

  :config
  (shackle-mode 1))


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
