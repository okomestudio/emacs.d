;;; init-startup.el --- Startup  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package bytecomp
  :ensure nil
  :custom
  (byte-compile-warnigns '(cl-functions)))

(use-package emacs
  :custom
  (vc-follow-symlinks t)

  :init
  (defun ts/setup-frame (frame)
    (progn
      (when (display-graphic-p)
        (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

      ;; Fonts
      (defun ts/get-display-width ()
        "Get the pixel with per display."
        (when window-system
          (let ((monn (length (display-monitor-attributes-list))))
            (/ (display-pixel-width) monn))))

      (defvar ts/display-width (ts/get-display-width))
      (defvar ts/font-size (if (and ts/display-width
                                    (> ts/display-width 2550))
                               18 12))

      (when ts/font-size
        (create-fontset-from-ascii-font
         (format "Hack:weight=normal:slant=normal:size=%d" ts/font-size)
         nil "hackandjp")
        (set-fontset-font "fontset-hackandjp"
                          'unicode
                          (font-spec :family "Noto Sans Mono CJK JP")
                          nil
                          'append)
        (add-to-list 'default-frame-alist '(font . "fontset-hackandjp")))))

  (when window-system
    (setq select-enable-clipboard t))

  (if (daemonp)
      (add-hook 'after-make-frame-functions 'ts/setup-frame)
    (ts/setup-frame (selected-frame)))

  :bind
  (("C-x C-y" . (lambda ()
                  (interactive)
                  (insert (shell-command-to-string
                           (expand-file-name "bin/pbocr" user-emacs-directory)))))))

;; A RPC stack for the Emacs Lisp
(use-package epc
  :ensure t)

(use-package files
  :ensure nil

  :custom
  (make-backup-files nil)
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist '(("." . ts/backup-cache-dir)))
  ;(auto-save-file-name-transforms '((".*" ts/backup-cache-dir t)))

  :init
  (defconst ts/backup-cache-dir (expand-file-name "~/.cache/emacs-backups-2"))
  (ensure-directory-exists ts/backup-cache-dir))

(use-package fringe
  :ensure nil
  :init
  (fringe-mode 0))

(use-package hippie-exp
  :ensure nil
  :init
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

(use-package icomplete
  :ensure nil
  :init
  (fido-mode 1))

(use-package imenu
  :ensure nil
  :init
  (global-set-key (kbd "M-i") 'imenu))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-scroll-amount '(3 ((shift) . 1))))

(use-package simple
  :ensure nil

  :bind
  (("<f5>" . 'ts/revert-buffer-no-confirm)
   ("C-S-o" . 'ts/newline-above)
   ("C-c C-x *" . 'ts/insert-zero-width-space)
   ("C-o" . 'ts/newline-below)
   ("M-Q" . 'ts/unfill-paragraph))

  :hook
  (before-save . delete-trailing-whitespace)

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

  (setq-default indent-tabs-mode nil)

  (setq apropos-sort-by-scores t)
  (setq case-fold-search t)             ; in C source code
  (setq sentence-end-double-space nil)  ; in paragraphs.el
  (setq tab-always-indent t)            ; in indent.el
  (setq tab-width 2)                    ; in C source code
  )

(use-package startup
  :ensure nil
  :no-require t
  :custom
  (column-number-mode t)
  (inhibit-splash-screen nil)
  (inhibit-startup-screen nil)
  (size-indication-mode t)

  :init
  (setq-default scroll-bar-width 6)
  (setq ring-bell-function 'ignore)     ; Disable beeping (in C source code)
  (prefer-coding-system 'utf-8)         ; Use UTF-8 when possible
)

(use-package tooltip
  :ensure nil
  :init
  (tooltip-mode 1))

;; C-c <left> undoes, C-c <right> redoes
(use-package winner
  :ensure nil
  :init
  (winner-mode 1))

;; frame-comds is used to add C-x o and C-x p to go back and forth between windows.
(use-package frame-cmds
  :ensure nil

  :bind
  (("C-x o" . (lambda () (interactive) (other-window-or-frame 1)))
   ("C-x p" . (lambda () (interactive) (other-window-or-frame -1)))
   ("M-o" . 'other-window-or-frame))

  :init
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/frame-fns.el")
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/frame-cmds.el")

  (setq-default frame-title-format '("" "%f - Emacs")))

;; Add node_modules to your exec-path
(use-package add-node-modules-path)

;; Invoke direnv to obtain the environment for the current file, then update the
;; emacs variables process-environment and exec-path.
(use-package direnv
  :config
  (direnv-mode)
  :ensure-system-package
  ((direnv . "sudo apt install direnv")))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (exec-path-from-shell-initialize))

;; Smart garbage collection
(use-package gcmh
  :defer nil
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

(provide 'init-startup)
;;; init-startup.el ends here
