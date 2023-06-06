;;; init-themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; LOAD THEME

(defun ts/configure-theme (theme)
  "Configure and load THEME."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme theme t))))
    (load-theme theme t)))

;; (use-package professional-theme
;;   :init (ts/configure-theme 'professional))

;; (use-package modus-themes
;;   :ensure modus-themes
;;   :init (ts/configure-theme 'modus-operandi))

(use-package spacemacs-theme
  :defer t
  :init
  (custom-set-variables '(spacemacs-theme-custom-colors
                          '((base . "#322938") ; #655370 for light, true-color
                            )))
  (ts/configure-theme 'spacemacs-light))


;;; THEME MISC.

(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers.
  :custom (solaire-mode-real-buffer-fn 'ts/solaire-mode-real-buffer-p)
  :init
  (defun ts/solaire-mode-real-buffer-p ()
    (if (string-prefix-p "*elfeed" (buffer-name))
        't (solaire-mode-real-buffer-p)))

  (solaire-global-mode +1))

(use-package pos-tip
  ;; Show tooltip at point.
  :init
  (setq pos-tip-background-color "white")
  (if (and (boundp 'ts/font-size) ts/font-size)
      (setq pos-tip-internal-border-width
            (truncate (* ts/font-size 1.5)))))

(use-package rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces according to
  ;; their depth.
  :hook (prog-mode . rainbow-delimiters-mode))

;; MODE-LINE

(use-package doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :demand t

  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-height 1)
  (doom-modeline-minor-modes t)
  (doom-modeline-vcs-max-length 50)

  :preface
  (defun my-doom-modeline-setup ()
    (doom-modeline-mode +1))

  ;; :init
  ;; (my-doom-modeline-setup)

  :hook (after-init . doom-modeline-mode)
  )

(use-package minions
  ;; A minor-mode menu for the mode line.
  :custom (minions-direct '(projectile-mode))
  :config (minions-mode 1))

;; MISC.

(use-package hl-line
  ;; Highlight the current line.
  :ensure nil
  :config (set-face-attribute 'hl-line nil :inherit nil :background "pale goldenrod")
  :init (global-hl-line-mode +1))

(use-package highlight-indent-guides
  :disabled
  :hook ((prog-mode) . highlight-indent-guides-mode)

  :config
  ;; (setq highlight-indent-guides-auto-enabled nil)
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\┆
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0
        highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-character-face "light yellow")
  (set-face-background 'highlight-indent-guides-top-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-top-character-face "gray"))

(use-package fringe
  :straight nil
  :init (fringe-mode '(8 . 4)))

(provide 'init-themes)
;;; init-themes.el ends here
