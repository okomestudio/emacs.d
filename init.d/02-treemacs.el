;;; 02-treemacs.el --- treemacs  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Treemacs and related utilities.
;;
;;; Code:

(use-package treemacs
  :bind
  (([f8] . ok-treemacs-show)
   ([mouse-1] . treemacs-single-click-expand-action)
   ("M-0" . treemacs-select-window))

  :custom
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-no-png-images t) ;; set to nil for image icons
  (treemacs-show-cursor t)
  (treemacs-wide-toggle-width 80)
  (treemacs-width 25)

  :config
  (use-package cfrs
    ;; A simple alternative to read-string that allows reading input via a small
    ;; child-frame spawned at the position of the cursor.
    :defer t)

  (defun ok-treemacs-show ()
    (interactive)
    (push-mark)
    (treemacs)
    (pop-global-mark))

  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-project-follow-mode t)
  (setq treemacs--project-follow-delay 1.0
        treemacs--project-follow-timer nil))


(use-package treemacs-nerd-icons
  :disabled ;; NOTE(2023-12-29): Disabled since char alignment is poor
  :config (treemacs-load-theme "nerd-icons"))


(use-package treemacs-all-the-icons
  :disabled ;; NOTE(2023-05-11): Disabled due to some formatting issue.
  :after (all-the-icons)
  :custom (all-the-icons-scale-factor 1.0)
  :config
  (treemacs-load-theme "all-the-icons")
  (treemacs-resize-icons 20))


(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))


(use-package treemacs-magit
  :after (treemacs magit))


(use-package treemacs-projectile
  :after (treemacs projectile))


(use-package lsp-treemacs
  :after (lsp-mode)
  :bind
  (([f6] . lsp-treemacs-errors-list)
   ([f7] . lsp-treemacs-symbols)))

;; Local Variables:
;; nameless-aliases: (("" . "ok"))
;; End:
;;; 02-treemacs.el ends here
