;;; themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs themes, visuals, colors, etc.
;;
;; See:
;;
;;   - https://emacsthemes.com/
;;
;;; Code:

(defvar themes-default-theme 'flexoki-themes-light
  "Default theme at startup.")

(load (ok-expand-lisp "themes-flexoki"))

;; The following utility functions assume that one and only one theme
;; gets loaded at any time.

(defvar after-load-theme-hook nil
  "Hooks to run after `load-theme'.")

(advice-add #'load-theme :around
            (lambda (orig-fun theme &optional no-confirm no-enable)
              (mapc #'disable-theme custom-enabled-themes)
              (funcall orig-fun theme no-confirm no-enable)
              (run-hooks 'after-load-theme-hook)))

;; MINOR THEME ADJUSTMENTS

(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers.
  :custom (solaire-mode-real-buffer-fn 'solaire-mode-ok--real-buffer-p)
  :config
  (defun solaire-mode-ok--real-buffer-p ()
    (cond ((string-prefix-p "*elfeed" (buffer-name)) t)
          ((string-prefix-p "*Colors*" (buffer-name)) nil)
          (t (solaire-mode-real-buffer-p))))

  (solaire-global-mode +1))

(use-package rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces according to
  ;; their depth.
  :hook (prog-mode . rainbow-delimiters-mode))

;; INDENTATION

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom ((indent-bars-color '(highlight :face-bg t :blend 0.2))
           (indent-bars-treesit-support t)
           (indent-bars-prefer-character "|")  ; github.com/jdtsmith/indent-bars/issues/3
           (indent-bars-width-frac 0.1)
           (indent-bars-pad-frac 0.1))
  :hook (prog-mode . indent-bars-mode))

;; MISC.

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

;; LOADER

(defun themes--initialize (theme)
  "Set the THEME to load at Emacs startup."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme theme t))))
    ;; (add-hook 'after-init-hook (lambda () (load-theme theme t)))
    (load-theme theme t)))

(load (ok-expand-lisp "themes-modeline.el"))

(themes--initialize themes-default-theme)

;;; themes.el ends here
