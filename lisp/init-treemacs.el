;;; init-treemacs.el --- Treemacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :after (cfrs treemacs-all-the-icons)
  :defer t

  :bind
  (([f8] . treemacs)
   ([mouse-1] . treemacs-single-click-expand-action))

  :custom
  (treemacs--project-follow-delay 0.5)
  (treemacs--project-follow-time nil)
  (treemacs-collapse-dirs 0)
  (treemacs-expand-after-init t)
  (treemacs-file-event-delay 100)
  (treemacs-file-follow-delay 0.1)
  (treemacs-follow-after-init t)
  (treemacs-is-never-other-window nil)
  (treemacs-missing-project-action 'keep)
  (treemacs-no-png-images nil)
  (treemacs-recenter-after-project-expand 'on-visibility)
  (treemacs-recenter-after-project-jump nil)
  (treemacs-show-cursor t)
  (treemacs-show-hidden-files nil)
  (treemacs-width 35)
  (treemacs-width-is-initially-locked nil)

  :config
  (defun ts/treemacs-resize ()
    "TBD"
    (interactive)
    (treemacs-select-window)
    (enlarge-window-horizontally 10)
    (message "%d" treemacs-width)
    (message "w %d" (window-size) )
    (treemacs-select-window))

  (when window-system
    (setq treemacs-indentation 2
          treemacs-space-between-root-nodes nil
          treemacs-width 35))

  ;; Add any files to be ignored
  (with-eval-after-load 'treemacs
    (defun ts/treemacs-ignore (filename absolute-path)
      (or (string-match-p "\\.vwxyz$" filename)))
    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore))

  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode nil)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-all-the-icons
  :after (all-the-icons)
  :custom (all-the-icons-scale-factor 1.0))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package cfrs
  ;; Child Frame Read String
  ;; -----------------------
  ;; https://github.com/Alexander-Miller/cfrs
  )

(provide 'init-treemacs)
;;; init-treemacs.el ends here
