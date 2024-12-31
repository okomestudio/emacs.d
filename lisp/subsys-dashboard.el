;;; subsys-dashboard.el --- Dashboard Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the dashboard subsystem.
;;
;;; Code:

(use-package dashboard
  :demand t
  :bind ("C-c d" . dashboard-open)
  :custom ((dashboard-agenda-release-buffers t)
           (dashboard-center-content t)
           (dashboard-display-icons-p t)
           (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
           (dashboard-icon-type 'nerd-icons)
           (dashboard-items '((recents . 5)
                              (projects . 3)
                              (bookmarks . 3)
                              (registers . 3)
                              (apps . 4)
                              (agenda . 5)))
           (dashboard-item-shortcuts '((agenda . "a")
                                       (apps . "t")
                                       (bookmarks . "m")
                                       (projects . "p")
                                       (recents . "r")
                                       (registers . "e")))
           (dashboard-item-names
            '(("Recent Files:" . "Recent Files (C-x r F):")
              ("Projects:" . "Projects (C-c p p):")
              ("Bookmarks:" . "Bookmarks (C-x r l):")))
           (dashboard-projects-backend 'projectile)
           (dashboard-startup-banner 'logo)
           (dashboard-set-file-icons t)
           (dashboard-set-footer nil)
           (dashboard-set-heading-icons t)
           (dashboard-set-init-info t)
           (dashboard-set-navigator t))
  :config
  ;; `apps' - frequently used apps
  ;;
  ;; TODO(2024-12-31): Fix shortcut to apps.
  ;;
  (defcustom dashboard-app-list
    '(("Elfeed (elfeed)" (elfeed))
      ("Tetris (tetris)" (tetrisk)))
    "List of apps to show in dashboard.
Each item is (name form).")

  (defun dashboard-apps-gen (list-size)
    (dashboard-insert-heading
     "Apps:"
     nil
     (nerd-icons-octicon "nf-oct-apps"
                         :height 1.2
                         :v-adjust 0.0
                         :face 'dashboard-heading))
    (dashboard-insert-section
     ""
     dashboard-app-list
     list-size
     'apps
     (dashboard-get-shortcut 'apps)
     `(lambda (&rest _) ,(cadr el))
     (car el)))
  ;; (add-to-list 'dashboard-item-shortcuts '(apps . "b"))
  (add-to-list 'dashboard-item-generators '(apps . dashboard-apps-gen))

  ;; `vocab' - word of the day custom widget
  (use-package wotd)

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

  ;; Start the hook:
  (dashboard-setup-startup-hook))

(provide 'subsys-dashboard)
;;; subsys-dashboard.el ends here
