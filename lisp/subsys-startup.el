;;; subsys-startup.el --- Startup Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Emacs startup subsystem.
;;
;;; Code:

;;; Common Emacs Lisp Libraries

(use-package buttercup)     ; behavior-driven Lisp testing
(use-package reformatter)   ; code formatter
(use-package uuid)

;;; Main Startup Config

(use-package emacs
  :straight nil
  :demand t
  :bind ( ("<f5>" . ok-buffer-revert-no-confirm)
          ("C-x 4 c" . clone-indirect-buffer-other-window)

          ;; These are bound by default, but made explicit for reminder:
          ("M-!" . shell-command)
          ("M-|" . shell-command-on-region)

          ;; Minimize instead of `save-buffers-kill-terminal' to avoid
          ;; accidental quit
          ("C-x C-c". iconify-frame) )
  :custom ((async-shell-command-buffer "new-buffer")
           (case-fold-search t)
           (compilation-scroll-output t)
           (confirm-kill-processes nil)
           (cursor-type 'box)
           (enable-recursive-minibuffers t)
           (initial-major-mode #'lisp-interaction-mode) ; `*scratch*' buffer
           (load-prefer-newer t)
           (next-error-message-highlight t)
           (tab-width 2)
           (uniquify-buffer-name-style 'forward)
           (use-dialog-box nil)
           (use-short-answers t)
           (vc-follow-symlinks t)
           (word-wrap-by-category t)

           ;; Basic editing
           (sentence-end-double-space nil) ; in paragraphs.el
           (show-paren-context-when-offscreen t)
           (show-paren-delay 0)
           (size-indication-mode nil)

           ;; File related config
           (auto-save-default nil)
           (backup-by-copying t)
           (create-lockfiles nil)
           (history-length 25)
           (make-backup-files nil)
           (require-final-newline nil)

           ;; Frame title (i.e., desktop window title)
           (frame-title-format
            '((:eval (when (project-current)
                       (format "[%s] " (file-name-nondirectory
                                        (directory-file-name
                                         (project-root (project-current)))))))
              (:eval (cond
                      ((buffer-file-name)
                       (abbreviate-file-name
                        (expand-file-name buffer-file-name)))
                      (t (buffer-name))))
              (:eval (format " - Emacs (%d)" (emacs-pid))))))
  :hook (before-save . ok-delete-trailing-whitespace)
  :init
  (defun ok-delete-trailing-whitespace ()
    (when (derived-mode-p 'text-mode 'prog-mode)
      (save-excursion
        (delete-trailing-whitespace))))

  (global-completion-preview-mode -1) ; favor corfu
  (column-number-mode 1)
  (global-so-long-mode 1)     ; mitigate perf on files with long lines
  (show-paren-mode 1)         ; highlight matching parens
  (subword-mode 1)
  (tooltip-mode 1)
  (setq-default indent-tabs-mode nil)

  ;; Multilingual environment
  (set-language-environment "UTF-8") ;; or, e.g., "Japanese"

  ;; avoid prefer-coding-system
  ;; (see https://github.com/takueof/.emacs.d/blob/master/init.el):
  (set-coding-system-priority 'utf-8)

  (setq-default buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8))

(use-package autorevert
  :straight nil
  :custom ((auto-revert-avoid-polling t))
  :hook (after-init . global-auto-revert-mode))

(use-package indent
  :straight nil
  :custom ((tab-always-indent 'complete)))

;;; Shell

(use-package add-node-modules-path
  ;; Add node_modules/.bin to exec-path.
  )

(use-package envrc
  :mode (("\\.envrc\\..*\\'" . envrc-file-mode))
  :ensure-system-package (direnv . "sudo apt install -y direnv")
  :hook (on-first-buffer . envrc-global-mode))

(use-package exec-path-from-shell
  ;; Make Emacs use the PATH set up by the user's shell.
  ;;
  ;; NOTE: The startup time will be substantially longer depending on
  ;; how much work gets done within startup shells (.bashrc,
  ;; .bash_profile, etc.). Make sure to keep them efficient once you
  ;; start seeing the warning from this package at startup.
  ;;
  :if (or (memq window-system '(mac ns pgtk x)) (daemonp))
  :custom ((exec-path-from-shell-arguments nil))
  :init (exec-path-from-shell-initialize))

(use-package keychain-environment
  ;; Loads keychain environment variables into emacs.
  :straight (:host github :repo "tarsius/keychain-environment")
  :config (keychain-refresh-environment))

;;; Misc.

(use-package anzu
  ;; Displays current and total matches information in the mode-line.
  :hook (on-first-input . global-anzu-mode))

(use-package dirvish)

(use-package repeat
  :straight nil
  :custom (repeat-echo-function #'repeat-echo-message)
  :hook (on-first-input . repeat-mode))

(use-package tramp
  :straight nil
  :custom (tramp-default-method "ssh"))

(use-package do-this-now
  :straight (do-this-now :host github
                         :repo "okomestudio/do-this-now.el")
  :custom ((alert-default-style 'libnotify)
           (do-this-now-idle-interval 600)
           (do-this-now-interval 2400)
           (do-this-now-message (format "Move away from computer! (PID: %s)"
                                        (emacs-pid)))
           (do-this-now-title "MOVE!!"))
  :hook (after-init . do-this-now-mode))

(provide 'subsys-startup)
;;; subsys-startup.el ends here
