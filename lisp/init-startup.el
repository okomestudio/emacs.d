;;; init-startup.el --- Startup  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :bind
  (("C-x C-y" . ts/pbocr))

  :custom
  (async-shell-command-buffer "new-buffer")
  (case-fold-search t)
  (compilation-scroll-output t)
  (enable-recursive-minibuffers t)
  (inhibit-splash-screen nil)
  (inhibit-startup-screen nil)
  (load-prefer-newer t)
  (next-error-message-highlight t)
  (read-process-output-max (* 1 1024 1024)) ; 1 mb
  (ring-bell-function 'ignore)              ; Disable beeping (in C source code)
  (tab-width 2)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (word-wrap-by-category t)

  :init
  (defun ts/pbocr ()
    "Run OCR on the image in clipboard and paste the text."
    (interactive)
    (insert (shell-command-to-string
             (expand-file-name "bin/pbocr" user-emacs-directory))))

  (when window-system
    (setq select-enable-clipboard t))

  (setq-default frame-title-format
                '((:eval (list (if (buffer-file-name)
                                   (abbreviate-file-name (expand-file-name buffer-file-name))
                                 (buffer-name) )))
                  " - Emacs"))
  (setq-default scroll-bar-width 6)

  ;; hack-dir-local-variables
  ;; ------------------------
  ;; Add an advice so that .dir-locals.el are looked for upward in the directory hierarchy.
  (defvar walk-dir-locals-upward t
    "Look for .dir-locals.el up in directory hierarchy.

If non-nil, evaluate .dir-locals.el files starting in the current
directory and going up. Otherwise they will be evaluated from the
top down to the current directory.")

  (defun ts/hack-dir-local-variables-advice (func)
    (if walk-dir-locals-upward
        (let ((dir-locals-file ".dir-locals.el")
              (original-buffer-file-name (buffer-file-name))
              (nesting (ts/locate-dominating-files
                        (or (buffer-file-name) default-directory) dir-locals-file)))
          (unwind-protect
              (dolist (name nesting)
                ;; make it look like a file higher up in the hierarchy is visited
                (setq buffer-file-name (concat name dir-locals-file))
                (funcall func))
            (setq buffer-file-name original-buffer-file-name)))
      (funcall func)))

  (advice-add 'hack-dir-local-variables :around #'ts/hack-dir-local-variables-advice)

  ;; Reload .dir-locals.el. See https://emacs.stackexchange.com/a/13096/599
  (defun ts/reload-dir-locals-for-current-buffer ()
    "Reload dir locals for the current buffer."
    (interactive)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer)))

  (defun ts/reload-dir-locals-for-all-buffer-in-this-directory ()
    "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
    (interactive)
    (let ((dir default-directory))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (equal default-directory dir)
            (ts/reload-dir-locals-for-current-buffer))))))

  (add-hook 'emacs-lisp-mode-hook
            (defun enable-autoreload-for-dir-locals ()
              (when (and (buffer-file-name)
                         (equal dir-locals-file
                                (file-name-nondirectory (buffer-file-name))))
                (add-hook 'after-save-hook
                          'ts/reload-dir-locals-for-all-buffer-in-this-directory
                          nil t))))

  ;; apropos
  ;; -------
  ;; Bind `C-h a` to apropos for ease of access.
  (global-set-key (kbd "C-h a") 'ts/apropos-prefix)
  (define-prefix-command 'ts/apropos-prefix nil "Apropos (a,d,f,i,l,v,C-v)")
  (define-key ts/apropos-prefix (kbd "a") 'apropos)
  (define-key ts/apropos-prefix (kbd "d") 'apropos-documentation)
  (define-key ts/apropos-prefix (kbd "f") 'apropos-command)
  (define-key ts/apropos-prefix (kbd "i") 'info-apropos)
  (define-key ts/apropos-prefix (kbd "l") 'apropos-library)
  (define-key ts/apropos-prefix (kbd "v") 'apropos-variable)
  (define-key ts/apropos-prefix (kbd "C-v") 'apropos-value)
  (setq apropos-sort-by-scores t))

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
  (show-paren-delay 0)
  (size-indication-mode t)
  (tab-always-indent t)                 ; in indent.el

  :init
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
  (set-language-environment "UTF-8")
  )

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

  :init
  (ensure-directory-exists ts/backup-cache-dir)

  :preface
  (defconst ts/backup-cache-dir (expand-file-name "~/.cache/emacs-backups")))


;; WINDOWS AND FRAMES

(use-package ace-window
  ;; Quickly switch windows in Emacs.
  :bind (("M-O" . 'ace-window))
  :custom (aw-dispatch-always t))

(use-package frame-cmds
  ;; Frame functions and commands.
  :ensure nil
  :bind (("M-o" . 'other-window-or-frame))

  :init
  (ensure-file-from-url "https://www.emacswiki.org/emacs/download/frame-fns.el")
  (ensure-file-from-url "https://www.emacswiki.org/emacs/download/frame-cmds.el"))

(use-package shackle
  ;; Enforce rules for popup windows.
  :disabled

  :config (shackle-mode 1)

  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 0.4)
  ;; (shackle-rules '(("*Warnings*"
  ;;                   :select nil :size 0.25)
  ;;                  (magit-status-mode
  ;;                   :align right :size 0.5 :inhibit-window-quit t :other t)))
  )

(use-package topsy
  ;; Simple sticky header showing definition beyond top of window.
  :disabled                             ; Because of conflict with lsp
  :quelpa (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook (prog-mode . topsy-mode))

(use-package yascroll
  ;; Yet Another Scroll Bar Mode.
  :init
  (ensure-file-from-github "emacsorphanage/yascroll/master/yascroll.el")

  ;; Enable only if no GUI:
  (if (not window-system)
      (if (daemonp)
          (add-hook 'after-make-frame-functions
                    (lambda (frame)
                      (with-selected-frame frame
                        (when (not window-system)
                          (global-yascroll-bar-mode +1)))))
        (global-yascroll-bar-mode +1))))


;; INPUT DEVICES

(use-package mwheel
  :straight nil
  :custom
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-scroll-amount '(3 ((shift) . 1))))


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
  :config (direnv-mode)
  :ensure-system-package ((direnv . "sudo apt install -y direnv")))

(use-package exec-path-from-shell
  ;; Make Emacs use the PATH set up by the user's shell.
  ;;
  ;; Ensure environment variables look the same in the user's shell.
  ;;
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config (exec-path-from-shell-initialize))

(use-package keychain-environment
  ;; Loads keychain environment variables into emacs.
  :ensure nil
  :init
  (ensure-file-from-github "tarsius/keychain-environment/master/keychain-environment.el")
  (require 'keychain-environment)
  (keychain-refresh-environment))


;; OPTIMIZATIONS

(use-package bytecomp
  :straight nil
  :custom (byte-compile-warnigns '(cl-functions)))

;; A RPC stack for the Emacs Lisp
;; (use-package epc
;;   :ensure t)

(use-package gcmh
  ;; The Garbage Collector Magic Hack.
  :defer nil
  :hook (after-init . gcmh-mode)

  :custom
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-idle-delay 5))


;; MISC.

(use-package dash
  ;; A modern list library for Emacs
  )

(use-package help-shortdoc-example
  ;; Display shortdoc examples to *Help* buffer.
  :straight nil
  :config (help-shortdoc-example-mode 1)
  :init
  (ensure-file-from-github "buzztaiki/help-shortdoc-example.el/main/help-shortdoc-example.el"))

(use-package restart-emacs
  ;; Restart emacs from within emacs.
  :defer t)

(use-package tramp
  :custom (tramp-default-method "ssh")
  :defer t)

(use-package which-key
  ;; Displays available keybindings in popup.

  :bind (("C-h W" . which-key-show-top-level))

  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(right bottom))

  :config
  (which-key-mode +1))

(provide 'init-startup)
;;; init-startup.el ends here
