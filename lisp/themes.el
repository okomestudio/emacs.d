;;; themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure custom themes.
;;
;; Use the following commands to switch themes.
;;
;;   - `ok-theme-enable-preset' to load a theme preset
;;   - `ok-theme-prepare' to load theme feature
;;   - `ok-theme-enable' to enable a theme
;;
;; See https://emacsthemes.com for available custom themes.
;;
;;; Code:

(require 'ok)

(setopt ok-theme-custom-themes
        '((doom-themes . "lisp/themes/themes-doom")
          (flexoki-themes . "lisp/themes/themes-flexoki")
          (kanagawa-themes . "lisp/themes/themes-kanagawa")
          (nano-theme . "lisp/themes/themes-nano")
          (ok-org-themes . "lisp/themes/themes-ok-org")
          (spacemacs-theme . "lisp/themes/themes-spacemacs")))

(setopt ok-theme-presets '((dark . ((spacemacs-dark . spacemacs-theme)
                                    (ok-org-modern . ok-org-themes)))
                           (light . ((spacemacs-light . spacemacs-theme)
                                     (ok-org-modern . ok-org-themes)))))

(ok-theme-prepare-enable-on-startup 'light)

;;; Modeline

(load (ok-file-expand-lisp "themes-modeline.el"))

;;; Minor Theme Adjustments

(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers by element
  ;; color.
  :custom (solaire-mode-real-buffer-fn 'solaire-mode-ok--real-buffer-p)
  :hook (after-init . solaire-global-mode)
  :config
  (defun solaire-mode-ok--real-buffer-p ()
    "Return non-nil for a real buffer, nil otherwise."
    (cond ((derived-mode-p '(dashboard-mode)) t)
          ((string-prefix-p "*elfeed" (buffer-name)) t)
          ((string-prefix-p "*Colors*" (buffer-name)) nil)
          (t (solaire-mode-real-buffer-p)) ; fallback to the default
          )))

(use-package rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces according to
  ;; their depth.
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  ;; Colorize color names in buffers.
  :hook ((emacs-lisp-mode lisp-data-mode) . rainbow-mode))

;;; Indentation

(use-package indent-bars
  :straight (indent-bars :type git
                         :host github
                         :repo "jdtsmith/indent-bars")
  :custom ((indent-bars-color '(highlight :face-bg t :blend 0.2))
           (indent-bars-no-descend-lists nil)
           (indent-bars-treesit-support t)
           (indent-bars-prefer-character "|") ; github.com/jdtsmith/indent-bars/issues/3
           (indent-bars-width-frac 0.1)
           (indent-bars-pad-frac 0.1))
  :hook ((prog-mode . indent-bars-mode)
         ((emacs-lisp-mode
           lisp-data-mode) . (lambda () (setq-local
                                         indent-bars-no-descend-lists t)))))

;;; Window Dividers

(use-package frame
  :straight nil
  :hook (enable-theme-functions . window-divider-mode)
  :config
  ;; For a thin border, set dividers to the same foreground color:
  (let ((fg (face-attribute 'window-divider :foreground)))
    (set-face-attribute 'window-divider-first-pixel nil :foreground fg)
    (set-face-attribute 'window-divider-last-pixel nil :foreground fg)))

;;; Misc.

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
         (enable-theme-functions . hl-line-ok--background-init)
         (solaire-mode . hl-line-ok--background-solaire-mode))
  :config
  (defun hl-line-ok--background (&optional face)
    (require 'ok)
    (ok-face-color-scale (face-attribute face :background)
                         (pcase (frame-parameter nil 'background-mode)
                           ('dark 1.03)
                           ('light 0.97))))

  (defun hl-line-ok--background-init (theme)
    "Initialize `hl-line' face using the `default' face."
    (set-face-attribute 'hl-line nil
                        :background (hl-line-ok--background 'default)))

  (defun hl-line-ok--background-solaire-mode ()
    "Toggle `hl-line' face attributes based on `solaire-mode' status."
    (if solaire-mode
        (let ((bg (hl-line-ok--background 'solaire-default-face)))
          (setq-local
           hl-line-ok--background-solaire-mode-cookie
           (face-remap-add-relative 'hl-line `(:background ,bg))))
      (when (and (buffer-local-boundp
                  'hl-line-ok--background-solaire-mode-cookie
                  (current-buffer))
                 hl-line-ok--background-solaire-mode-cookie)
        (face-remap-remove-relative hl-line-ok--background-solaire-mode-cookie)
        (setq-local hl-line-ok--background-solaire-mode-cookie nil))))

  (defun hl-line-ok--ad-disable-hl-line (fun &rest rest)
    "Disable `hl-line-mode' with `face-at-point'.
When `describe-face' is invoked interactively, the user is interested in
the face at point. When `hl-line-mode' is active, however, the face will
always point to `hl-line'. This advice temporarily disables
`hl-line-mode' so that the underlying face can be picked up by default."
    (let (result)
      (cond ((and (boundp 'global-hl-line-mode) global-hl-line-mode)
             (progn
               (global-hl-line-mode -1)
               (setq result (apply fun rest))
               (global-hl-line-mode 1)))
            ((and (boundp 'hl-line-mode) hl-line-mode)
             (progn
               (hl-line-mode -1)
               (setq result (apply fun rest))
               (hl-line-mode 1)))
            (t (setq result (apply fun rest))))
      result))
  (advice-add #'face-at-point :around #'hl-line-ok--ad-disable-hl-line))

(use-package pos-tip
  ;; Show tooltip at point.
  :custom ((pos-tip-border-width 0)
           (pos-tip-internal-border-width 4)

           ;; Not specifying these will render pos-tip texts in the
           ;; tooltip face colors:
           (pos-tip-foreground-color nil)
           (pos-tip-background-color nil)))

(provide 'themes)
;;; themes.el ends here
