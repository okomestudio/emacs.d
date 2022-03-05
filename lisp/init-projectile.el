;;; init-projectile.el --- Projectile  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :custom
  (projectile-project-search-path '(("~/github.com/" . 5)))
  (projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))

  :ensure-system-package
  ((fdfind . "sudo apt install fd-find")
   (ag . "sudo apt-get install silversearcher-ag"))

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  :init
  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-dir 'safe-local-variable #'stringp)
  (put 'projectile-project-configure-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-install-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-package-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp))

(use-package ag)

(provide 'init-projectile)
;;; init-projectile.el ends here
