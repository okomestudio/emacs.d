;;; themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up themes, visuals, colors, etc.
;;
;; See: https://emacsthemes.com for options.
;;
;;; Code:

(load (ok-expand-lisp "themes-flexoki"))   ; switches theme

;;; THEME LOADER

(defcustom themes-default nil
  "Default theme.")

(defun themes--prepare-load (theme)
  "Prepare THEME to load at Emacs startup."
  (if (daemonp)
      (progn
        (defun load-theme--when-run-as-daemon (frame)
          (with-selected-frame frame
            (load (ok-expand-lisp "themes-modeline.el"))
            (load-theme theme t)))
        (add-hook 'after-make-frame-functions #'load-theme--when-run-as-daemon))
    (defun load-theme--after-init ()
      (load (ok-expand-lisp "themes-modeline.el"))
      (load-theme theme t))
    (add-hook 'after-init-hook #'load-theme--after-init)))

(defvar after-load-theme-hook nil
  "Hooks to run after `load-theme'.")

(defun load-theme--ad (orig-fun &rest _)
  "Disable all themes before ORIG-FUN and run after-load hooks."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fun _)
  (run-hooks 'after-load-theme-hook))

(advice-add #'load-theme :around #'load-theme--ad)

;;; MINOR THEME ADJUSTMENTS

(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers.
  :custom (solaire-mode-real-buffer-fn 'solaire-mode-ok--real-buffer-p)
  :hook (after-load-theme . solaire-global-mode)
  :config
  (defun solaire-mode-ok--real-buffer-p ()
    (cond ((string-prefix-p "*elfeed" (buffer-name)) t)
          ((string-prefix-p "*Colors*" (buffer-name)) nil)
          (t (solaire-mode-real-buffer-p)))))

(use-package rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces according to
  ;; their depth.
  :hook (prog-mode . rainbow-delimiters-mode))

;;; INDENTATION

(use-package indent-bars
  :straight (indent-bars :type git
                         :host github
                         :repo "jdtsmith/indent-bars")
  :custom ((indent-bars-color '(highlight :face-bg t :blend 0.2))
           (indent-bars-treesit-support t)
           (indent-bars-prefer-character "|")  ; github.com/jdtsmith/indent-bars/issues/3
           (indent-bars-width-frac 0.1)
           (indent-bars-pad-frac 0.1))
  :hook (prog-mode . indent-bars-mode))

;;; MISC.

(use-package olivetti
  ;; Emacs minor mode to automatically balance window margins
  )

(use-package ansi-color
  :straight nil
  :hook (compilation-filter . ansi-color-ok--apply)
  :config
  (defun ansi-color-ok--apply ()
    (when (derived-mode-p 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(use-package hl-line
  ;; Highlight the current line.
  :straight nil
  :hook ((on-first-input . global-hl-line-mode)
         (after-load-theme . hl-line-ok--adjust))
  :config
  (require 'ok)

  (defun hl-line-ok--adjust ()
    (let* ((mode (frame-parameter nil 'background-mode))
           (scale (if (string= mode "dark") 1.03 0.97))
           (bg (face-attribute 'default :background))
           (bg-hl (ok-face-color-scale bg scale)))
      (set-face-attribute 'hl-line nil :background bg-hl))))

(use-package pos-tip
  ;; Show tooltip at point.
  :custom ((pos-tip-background-color "#dddddd")
           (pos-tip-border-width 5)
           (pos-tip-internal-border-width 5)))

(themes--prepare-load themes-default)

(provide 'themes)
;;; themes.el ends here
