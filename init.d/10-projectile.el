;;; 10-projectile.el --- Projectile  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Projectile.
;;
;;; Code:

(use-package projectile
  :bind-keymap
  ("s-p" . projectile-command-map) ;; "s-" is "super"
  ("C-c p" . projectile-command-map)

  :custom
  (projectile-auto-discover nil)
  (projectile-enable-caching nil)
  (projectile-git-fd-args "-H -0 -E .git -tf")
  (projectile-indexing-method 'alien)
  (projectile-mode-line-function '(lambda ()
                                    (format " [%s]" (projectile-project-name))))
  (projectile-project-root-functions '(projectile-root-local
                                       projectile-root-bottom-up
                                       projectile-root-top-down
                                       projectile-root-top-down-recurring))
  (projectile-project-search-path `(,(expand-file-name "straight/repos/"
                                                       user-emacs-directory)
                                    (,(expand-file-name "github.com/"
                                                        (getenv "HOME")) . 2)))

  :ensure-system-package
  (ag . "sudo apt install -y silversearcher-ag")
  (fdfind . "sudo apt install -y fd-find")

  :preface
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)
  (put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-install-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-package-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp)

  :config
  (projectile-mode +1)

  ;; Doing this in :bind doesn't appear to work:
  (keymap-global-set "C-x 4 p" (lambda ()
                                 (interactive)
                                 (other-window -1)
                                 (projectile-switch-project)
                                 (other-window +1))))

;;; 10-projectile.el ends here
