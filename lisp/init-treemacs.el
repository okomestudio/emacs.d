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
  (treemacs-collapse-dirs 0)
  (treemacs-file-event-delay 500)
  (treemacs-follow-after-init t)
  (treemacs-missing-project-action 'keep)
  (treemacs-no-png-images nil)
  (treemacs-recenter-after-project-expand 'on-visibility)
  (treemacs-recenter-after-project-jump nil)
  (treemacs-show-cursor t)
  (treemacs-show-hidden-files nil)
  (treemacs-width 45)
  (treemacs-width-is-initially-locked t)

  :config
  (when window-system
    (setq treemacs-indentation 2
          treemacs-is-never-other-window nil
          treemacs-space-between-root-nodes nil
          treemacs-width 45))

  ;; Add any files to be ignored
  (with-eval-after-load 'treemacs
    (defun ts/treemacs-ignore (filename absolute-path)
      (or (string-match-p "\\.vwxyz$" filename)))
    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore))

  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-all-the-icons
  :after (all-the-icons))

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
