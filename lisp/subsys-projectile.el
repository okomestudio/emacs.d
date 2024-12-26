;;; subsys-projectile.el --- Projectile Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the projectile subsystem.
;;
;;; Code:

(use-package projectile
  :bind-keymap (("s-p" . projectile-command-map)  ; "s-" is "super"
                ("C-c p" . projectile-command-map))
  :custom ((projectile-auto-discover nil)
           (projectile-enable-caching nil)
           (projectile-git-fd-args "-H -0 -E .git -tf")
           (projectile-ignored-projects '("~/" "~/.pyenv/"))
           (projectile-indexing-method 'alien)
           (projectile-mode-line-function
            '(lambda ()
               (format " [%s]" (projectile-project-name))))
           (projectile-project-root-functions
            '(projectile-root-local
              projectile-root-bottom-up
              projectile-root-top-down
              projectile-root-top-down-recurring))
           (projectile-project-search-path
            `(,(locate-user-emacs-file "straight/repos/")
              (,(expand-file-name "github.com/"
                                  (getenv "HOME")) . 2))))
  :ensure-system-package
  (ag . "sudo apt install -y silversearcher-ag")
  (fdfind . "sudo apt install -y fd-find")

  :preface
  (ok-safe-local-variable-add projectile-project-compilation-cmd stringp
                              projectile-project-compilation-dir stringp
                              projectile-project-configure-cmd stringp
                              projectile-project-install-cmd stringp
                              projectile-project-package-cmd stringp
                              projectile-project-root stringp
                              projectile-project-run-cmd stringp
                              projectile-project-test-cmd stringp)
  :config
  (projectile-mode +1)

  ;; Doing this in :bind doesn't appear to work:
  (keymap-global-set "C-x 4 p" (lambda ()
                                 (interactive)
                                 (other-window -1)
                                 (projectile-switch-project)
                                 (other-window +1))))

(provide 'subsys-projectile)
;;; subsys-projectile.el ends here
