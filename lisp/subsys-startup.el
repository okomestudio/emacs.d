;;; subsys-startup.el --- Startup Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Emacs startup subsystem.
;;
;;; Code:

;; COMMON ELISP LIBRARIES

(use-package ok
  :straight (:host github :repo "okomestudio/ok.el")
  :demand t)
(use-package buttercup)     ; behavior-driven Lisp testing
(use-package dash)          ; a modern list library
(use-package reformatter)   ; code formatter
(use-package uuid)

;; MAIN STARTUP CONFIG

(use-package emacs
  :straight nil
  :demand t
  :bind (("<f5>" . ok-buffer-revert-no-confirm)
         ("C-S-o" . ok-edit-insert-newline-above)
         ("C-c i SPC" . ok-edit-insert-zero-width-space)
         ("C-c i s" . ok-edit-insert-section-delimiter)
         ("C-o" . ok-edit-insert-newline-below)
         ("C-x C-c". iconify-frame)  ; prevent accidental quit
         ("M-q" . ok-edit-fill-or-unfill-paragraph)) ; TODO: Use `prog-fill-reindent-defun'?
  :custom ((async-shell-command-buffer "new-buffer")
           (case-fold-search t)
           (compilation-scroll-output t)
           (confirm-kill-processes nil)
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

           ;; Frame
           (frame-title-format '((:eval
                                  (list (cond
                                         ((buffer-file-name)
                                          (abbreviate-file-name
                                           (expand-file-name buffer-file-name)))
                                         (t (buffer-name)))))
                                 " - Emacs")))

  :hook (before-save . ok-delete-trailing-whitespace)

  :preface
  (put 'eval 'safe-local-variable #'listp)

  :init
  (defun ok-delete-trailing-whitespace ()
    (when (derived-mode-p 'text-mode 'prog-mode)
      (save-excursion
        (delete-trailing-whitespace))))

  (global-completion-preview-mode 1)  ; TODO(2024-12-25): try for a while
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
  :init (global-auto-revert-mode 1))

(use-package indent
  :straight nil
  :custom ((tab-always-indent 'complete)))

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
  :disabled
  :ensure-system-package (direnv . "sudo apt install -y direnv")
  :config (direnv-mode))

(use-package envrc
  :hook ((change-major-mode-after-body . envrc-mode)))

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

;; TREE-SITTER

(use-package treesit
  :straight nil
  :demand t)

(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; MISC.

(use-package anzu
  ;; Displays current and total matches information in the mode-line.
  :hook (on-first-input . (lambda () (global-anzu-mode 1))))

(use-package tramp
  :straight nil
  :custom (tramp-default-method "ssh"))

(use-package do-this-now
  :straight (do-this-now
             :host github
             :repo "okomestudio/do-this-now.el")
  :custom ((alert-default-style 'notifications)
           (do-this-now-interval 2400)
           (do-this-now-message "Move away from computer!")
           (do-this-now-title "MOVE!!"))
  :demand t)

(provide 'subsys-startup)
;;; subsys-startup.el ends here
