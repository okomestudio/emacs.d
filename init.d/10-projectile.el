;;; 10-projectile.el --- Projectile  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :defer t

  :bind
  (;
   :map ctl-x-4-map
   ("p" . (lambda ()
            (interactive)
            (other-window -1)
            (projectile-switch-project)
            (other-window +1)))

   :map projectile-mode-map
   ("s-p" . projectile-command-map) ; "s-" is "super"
   ("C-c p" . projectile-command-map))

  :custom
  (projectile-auto-discover nil)
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien)
  (projectile-mode-line-function '(lambda ()
                                    (format " [%s]" (projectile-project-name))))
  (projectile-project-root-functions '(projectile-root-local
                                       projectile-root-bottom-up
                                       projectile-root-top-down
                                       projectile-root-top-down-recurring))
  (projectile-project-search-path '("~/.config/emacs/" ("~/github.com/" . 2)))

  ;; Remove --strip-cwd-prefix till `fd` version 8.3.0+ is available:
  ;; NOTE(2023-12-31): fd-find is at 8.6.0 in Bookworm so the below can be removed.
  ;; (projectile-generic-command "fd . -0 --type f --color=never")
  (projectile-git-fd-args "-H -0 -E .git -tf")

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
  (projectile-mode +1))

;;; 10-projectile.el ends here
