;;; init-treemacs.el --- Treemacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package treemacs
  :after (cfrs)
  :defer t

  :bind
  (([f8] . init-treemacs--treemacs)
   ([mouse-1] . treemacs-single-click-expand-action))

  :custom
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-no-png-images t) ;; set to nil for image icons
  (treemacs-persist-file "~/.config/emacs/.cache/treemacs-persist")
  (treemacs-show-cursor t)
  (treemacs-wide-toggle-width 75)
  (treemacs-width 30)

  :config
  (defun init-treemacs--treemacs ()
    (interactive)
    (push-mark)
    (treemacs)
    (pop-global-mark))

  (with-eval-after-load 'treemacs
    (defun init-treemacs--treemacs-ignore (filename absolute-path)
      "Define a predicate function for files to be ignored from view in Treemacs."
      (or
       ;; probably just for testing
       (string-match-p "\\.vwxyz$" filename)
       ;; all org-roam node files except templates
       (and (string-match-p ".*/roam/.*" absolute-path)
            (not (string-match-p ".*/roam/template/?.*" absolute-path)))))

    (add-to-list 'treemacs-ignored-file-predicates #'init-treemacs--treemacs-ignore))

  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t)

  (treemacs-project-follow-mode t)
  (setq treemacs--project-follow-delay 1.5
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


(use-package cfrs
  ;; A simple alternative to read-string that allows reading input via a small
  ;; child-frame spawned at the position of the cursor.
  )


(provide 'init-treemacs)
;;; init-treemacs.el ends here
