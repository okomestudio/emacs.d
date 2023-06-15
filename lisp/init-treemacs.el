;;; init-treemacs.el --- Treemacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treemacs
  :after (cfrs)
  :defer t

  :bind
  (([f8] . ts/treemacs)
   ([mouse-1] . treemacs-single-click-expand-action))

  :custom
  ;; (treemacs-collapse-dirs 0)
  ;; (treemacs-display-current-project-exclusively t)
  ;; (treemacs-expand-after-init t)
  ;; (treemacs-file-event-delay 500)
  ;; (treemacs-file-follow-delay 1.01)
  ;; (treemacs-follow-after-init t)
  ;; (treemacs-indent-guide-style 'line)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  ;; (treemacs-is-never-other-window nil)
  ;; (treemacs-missing-project-action 'keep)
  (treemacs-no-png-images t)            ; set to nil for image icons
  (treemacs-persist-file "~/.config/emacs/.cache/treemacs-persist")
  ;; (treemacs-recenter-after-project-expand 'on-visibility)
  ;; (treemacs-recenter-after-project-jump nil)
  (treemacs-show-cursor t)
  ;; (treemacs-show-hidden-files nil)
  (treemacs-wide-toggle-width 75)
  (treemacs-width 30)
  ;; (treemacs-width-is-initially-locked nil)

  :config
  (defun ts/treemacs ()
    (interactive)
    (push-mark)
    (treemacs)
    (pop-global-mark))

  (defun ts/treemacs-resize ()
    "TBD"
    (interactive)
    (treemacs-select-window)
    (enlarge-window-horizontally 10)
    (message "%d" treemacs-width)
    (message "w %d" (window-size) )
    (treemacs-select-window))

  ;; (when window-system
  ;;   (setq treemacs-indentation 2
  ;;         treemacs-indentation-string " "
  ;;         treemacs-width 35))

  (with-eval-after-load 'treemacs
    (defun ts/treemacs-ignore (filename absolute-path)
      "Define a predicate function for files to be ignored from view in Treemacs."
      (or
       ;; probably just for testing
       (string-match-p "\\.vwxyz$" filename)
       ;; all org-roam node files except templates
       (and (string-match-p ".*/roam/.*" absolute-path)
            (not (string-match-p ".*/roam/template/?.*" absolute-path)))))

    (add-to-list 'treemacs-ignored-file-predicates #'ts/treemacs-ignore))

  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t)

  ;; NOTE(2023-05-11): Disabled due to some formatting issue.
  ;; (treemacs-load-theme "all-the-icons")

  (treemacs-project-follow-mode t)
  (setq treemacs--project-follow-delay 1.5
        treemacs--project-follow-timer nil)

  ;; (setq  (treemacs--project-follow-delay 1.0)
  ;;        (treemacs--project-follow-timer nil)
  ;;        )

  (treemacs-resize-icons 20)
  )

(use-package treemacs-nerd-icons
  :disabled
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-all-the-icons
  ;; NOTE(2023-05-11): Disabled due to some formatting issue.
  :disabled
  :after (all-the-icons)
  :custom (all-the-icons-scale-factor 1.0))

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
