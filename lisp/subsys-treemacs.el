;;; subsys-treemacs.el --- Treemacs Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Treemacs subsystem.
;;
;;; Code:

(use-package treemacs
  :bind (([f8] . treemacs-ok-toggle)
         ([mouse-1] . treemacs-single-click-expand-action)
         ("M-0" . treemacs-select-window))
  :custom ((treemacs-indentation 2)
           (treemacs-indentation-string " ")
           (treemacs-no-png-images t)  ; set to nil for image icons
           (treemacs-show-cursor t)
           (treemacs-wide-toggle-width 80)
           (treemacs-width 25))
  :config
  (use-package cfrs
    ;; A simple alternative to read-string that allows reading input via a small
    ;; child-frame spawned at the position of the cursor.
    :defer t)

  (defun treemacs-ok-toggle ()
    (interactive)
    (push-mark)
    (treemacs)
    (pop-global-mark))

  (treemacs-filewatch-mode 1)
  (treemacs-follow-mode 1)
  (treemacs-hide-gitignored-files-mode 1)
  (treemacs-fringe-indicator-mode 'always)

  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode 1))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))

  (treemacs-project-follow-mode 1)
  (setq treemacs--project-follow-delay 1.0
        treemacs--project-follow-timer nil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; NOTE (2024-12-25): A fancy icon set is available but each of
;; `treemacs-nerd-icons' (char alignment is poor) or
;; `treemacs-all-the-icons' (formatting is poor) has some issue.

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(provide 'subsys-treemacs)
;;; subsys-treemacs.el ends here
