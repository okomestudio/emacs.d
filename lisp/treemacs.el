;;; treemacs.el --- Treemacs  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Treemacs configuration.
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

  (defun treemacs-ok-toggle-maybe ()
    "Auto-toggle Treemacs in prog-mode and text-mode.
See https://gist.github.com/shegeley/cadcb16137fffd911f34ed6aaf673f54."
    (if (eq last-command 'treemacs)
        (set-frame-parameter
         (selected-frame) 'treemacs-forced-p
         (not (frame-parameter (selected-frame) 'treemacs-forced-p)))
      (unless (frame-parameter (selected-frame) 'treemacs-forced-p)
        (if (seq-find
             (lambda (x)
               (with-current-buffer (window-buffer x)
                 (derived-mode-p 'text-mode 'prog-mode)))
             (window-list))
            (unless (eq (treemacs-current-visibility) 'visible)
              (save-selected-window (treemacs)))
          (and (eq (treemacs-current-visibility) 'visible)
               (not (minibufferp))
               (delete-window (treemacs-get-local-window)))))))
  ;; (add-hook 'window-configuration-change-hook #'treemacs-ok-toggle-maybe)

  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))

  (treemacs-project-follow-mode t)
  (setq treemacs--project-follow-delay 1.0
        treemacs--project-follow-timer nil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

;;; treemacs.el ends here
