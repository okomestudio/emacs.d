;;; init-themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package professional-theme
  :disabled t

  :config
  (load-theme 'professional t))

(use-package spacemacs-common
  :ensure spacemacs-theme

  :config
  (load-theme 'spacemacs-light t))

(use-package all-the-icons
  :init
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts +1)))

(use-package cfrs)

(use-package highlight-indent-guides
  :disabled t
  :hook ((emacs-lisp-mode python-mode sh-mode) . highlight-indent-guides-mode)
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
(use-package minions
  :commands minions-mode
  :init (minions-mode 1))

;; Make certain buffers grossly incandescent
(use-package solaire-mode
  :init
  (solaire-global-mode +1))

;; Show tooltip at point
(use-package pos-tip
  :init
  (setq pos-tip-background-color "white")
  (if ts/font-size
      (setq pos-tip-internal-border-width
            (truncate (* ts/font-size 1.5)))))

(use-package treemacs
  :defer t
  :after (cfrs treemacs-all-the-icons)

  :bind
  (([f8] . treemacs)
   ([mouse-1] . treemacs-single-click-expand-action))

  :config
  (when window-system
    (setq treemacs-indentation 2
          treemacs-is-never-other-window t
          treemacs-space-between-root-nodes nil
          treemacs-width 40))

  (setq treemacs-collapse-dirs 0
        treemacs-file-event-delay 500
        treemacs-follow-after-init t
        treemacs-missing-project-action 'keep
        treemacs-no-png-images nil
        ;treemacs-recenter-after-project-jump nil
        ;treemacs-recenter-after-project-expand nil
        treemacs-show-cursor t
        treemacs-show-hidden-files nil)

  ;; (treemacs-display-current-project-exclusively)

  (with-eval-after-load 'treemacs
    (defun ts/treemacs-ignore-emacs (filename absolute-path)
      (or (string-match-p "\\.elc$" filename)
          (string-match-p "^#.*#$" filename)
          (string-match-p "~$" filename)))
    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore-emacs)
    (defun ts/treemacs-ignore-python (filename absolute-path)
      (or (string-match-p "\\(^\\|/\\)__pycache__$" filename)
          (string-match-p "\\(^\\|/\\)build$" filename)
          (string-match-p "\\(^\\|/\\)dist$" filename)
          (string-match-p "\\.egg-info$" filename)
          (string-match-p "\\.pyc$" filename)))
    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore-python))

  :init
  (treemacs-load-theme "all-the-icons")
  (treemacs-follow-mode -1))

(use-package treemacs-all-the-icons
  :after (all-the-icons))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

;; Display available keybindings in popup (github.com/justbur/emacs-which-key)
(use-package which-key
  :config
  (which-key-mode))

(provide 'init-themes)
;;; init-themes.el ends here
