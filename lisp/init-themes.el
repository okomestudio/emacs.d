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

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init (ts/configure-theme 'spacemacs-light))

;;; THEME MISC.

(use-package all-the-icons
  :init
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts +1)))

;; Make certain buffers grossly incandescent
(use-package solaire-mode
  :init (solaire-global-mode +1))

;; Show tooltip at point
(use-package pos-tip
  :init
  (setq pos-tip-background-color "white")
  (if (and (boundp 'ts/font-size) ts/font-size)
      (setq pos-tip-internal-border-width
            (truncate (* ts/font-size 1.5)))))

;; Highlights delimiters such as parentheses, brackets or braces according to
;; their depth (github.com/Fanael/rainbow-delimiters)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE-LINE

;; A fancy and fast mode-line inspired by minimalism design
;; (github.com/seagle0128/doom-modeline)
(use-package doom-modeline
  :demand t

  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-height 1)
  (doom-modeline-minor-modes t)
  (doom-modeline-vcs-max-length 50)

  :preface
  (defun my-doom-modeline-setup ()
    (doom-modeline-mode +1))

  :init
  (my-doom-modeline-setup))

;; A minor-mode menu for the mode line (github.com/tarsius/minions)
(use-package minions
  :custom (minions-direct '(projectile-mode))
  :config (minions-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC.

;; hl-line.el - Highlight the current line.
(use-package hl-line
  :ensure nil
  :config (set-face-attribute 'hl-line nil :inherit nil :background "LemonChiffon2")
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
  :ensure nil
  :init (fringe-mode '(8 . 4)))

(provide 'init-themes)
;;; init-themes.el ends here
