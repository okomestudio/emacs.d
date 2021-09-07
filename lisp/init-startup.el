;;; init-startup.el --- Startup  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package bytecomp
  :ensure nil
  :custom
  (byte-compile-warnigns '(cl-functions)))

(use-package emacs
  :bind
  (("C-x C-y" . ts/pbocr))

  :custom
  (async-shell-command-buffer "new-buffer")
  (compilation-scroll-output t)
  (vc-follow-symlinks t)

  :init
  (defun ts/pbocr ()
    "Run OCR on the image in clipboard and paste the text."
    (interactive)
    (insert (shell-command-to-string
             (expand-file-name "bin/pbocr" user-emacs-directory))))

  (defun ts/setup-frame (frame)
    (when (display-graphic-p)
      (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

    (when window-system
      ;; Fonts
      (defun ts/get-display-width ()
        "Get the pixel with per display."
        (let ((monn (length (display-monitor-attributes-list))))
          (/ (display-pixel-width) monn)))

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

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (ts/setup-frame frame))))
    (ts/setup-frame (selected-frame)))

  (when window-system
    (setq select-enable-clipboard t))

  ;; hack-dir-local-variables
  ;; ------------------------
  ;; Add an advice so that .dir-locals.el are looked for upward in the directory hierarchy.
  (defvar walk-dir-locals-upward t
    "Look for .dir-locals.el up in directory hierarchy.

If non-nil, evaluate .dir-locals.el files starting in the current
directory and going up. Otherwise they will be evaluated from the
top down to the current directory.")

  (defun hack-dir-local-variables-advice (func)
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

  (advice-add 'hack-dir-local-variables :around #'hack-dir-local-variables-advice)


  ;; apropos
  ;; -------

  ;; http://www.masteringemacs.org/articles/2011/08/04/full-text-searching-info-mode-apropos/#comment-1409
  (global-set-key (kbd "C-h a") 'ts/apropos-prefix)
  (define-prefix-command 'ts/apropos-prefix nil "Apropos (a,d,f,i,l,v,C-v)")
  (define-key ts/apropos-prefix (kbd "a") 'apropos)
  (define-key ts/apropos-prefix (kbd "d") 'apropos-documentation)
  (define-key ts/apropos-prefix (kbd "f") 'apropos-command)
  (define-key ts/apropos-prefix (kbd "c") 'apropos-command)
  (define-key ts/apropos-prefix (kbd "i") 'info-apropos)
  (define-key ts/apropos-prefix (kbd "l") 'apropos-library)
  (define-key ts/apropos-prefix (kbd "v") 'apropos-variable)
  (define-key ts/apropos-prefix (kbd "C-v") 'apropos-value)
  (setq apropos-sort-by-scores t))

;; topsy.el - Simple sticky header showing definition beyond top of window
;; -----------------------------------------------------------------------
;; https://github.com/alphapapa/topsy.el
(use-package topsy
  :disabled t
  :quelpa (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook (prog-mode . topsy-mode))

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
  (("M-o" . 'other-window-or-frame)
   ("M-O" . (lambda () (interactive) (other-window-or-frame -1))))

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
