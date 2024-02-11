;;; 85-dashboard.el --- Emacs Application Framework  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dashboard
  :demand t

  :custom
  (dashboard-agenda-release-buffers t)
  (dashboard-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((recents . 3)
                     (projects . 3)
                     (bookmarks . 3)
                     (agenda . 3)
                     ;; (vocab)
                     ))
  (dashboard-projects-backend 'projectile)
  (dashboard-startup-banner 'logo)
  (dashboard-set-file-icons t)
  (dashboard-set-footer nil)
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)

  :config
  (defun dashboard-vocab-gen (list-size)
    (dashboard-insert-heading "Word of the Day:"
                              nil
                              (nerd-icons-faicon "nf-fa-wordpress"
                                                 :height 1.2
                                                 :v-adjust 0.0
                                                 :face 'dashboard-heading))
    (save-excursion
      (let ((wotd-buffer-name "*Word of the Day: free-dictionary*"))
        (wotd-select 'free-dictionary)
        (switch-to-buffer wotd-buffer-name)
        (mark-whole-buffer)
        (copy-region-as-kill nil nil t)
        (kill-buffer wotd-buffer-name)))
    (insert "\n")
    (yank))

  (add-to-list 'dashboard-item-generators '(vocab . dashboard-vocab-gen))

  (dashboard-setup-startup-hook))

(use-package wotd)

;;; 85-dashboard.el ends here
