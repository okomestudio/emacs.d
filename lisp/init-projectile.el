;;; init-projectile.el --- Projectile  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map) ; "s-" is "super"
        ("C-c p" . projectile-command-map))

  :custom
  (projectile-auto-discover nil)
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien)
  (projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  (projectile-project-root-functions '(projectile-root-local
                                       projectile-root-bottom-up
                                       projectile-root-top-down
                                       projectile-root-top-down-recurring))
  (projectile-project-search-path '("~/.config/emacs/" ("~/github.com/" . 2)))

  :ensure-system-package
  ((ag . "sudo apt-get install silversearcher-ag")
   (fdfind . "sudo apt install fd-find"))

  :init
  (use-package ag)
  (projectile-mode +1)

  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)
  (put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-install-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-package-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp))

(provide 'init-projectile)
;;; init-projectile.el ends here
