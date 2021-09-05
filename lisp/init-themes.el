;;; init-themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init (ts/configure-theme 'spacemacs-light))

(use-package all-the-icons
  :init
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts +1)))

(use-package highlight-indent-guides
  :disabled t

  :hook
  ((emacs-lisp-mode python-mode sh-mode) . highlight-indent-guides-mode)

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

;; A minor-mode menu for the mode line
;; -----------------------------------
;; https://github.com/tarsius/minions
(use-package minions
  :custom (minions-direct '(projectile-mode))
  :config (minions-mode 1))

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

;; Display available keybindings in popup (github.com/justbur/emacs-which-key)
(use-package which-key
  :config (which-key-mode))

(provide 'init-themes)
;;; init-themes.el ends here
