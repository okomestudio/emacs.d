;;; subsys-projectile.el --- Projectile Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the projectile subsystem.
;;
;;; Code:

(use-package projectile
  :after project                        ; ensures the overriding of `C-x p'
  :bind-keymap ("C-x p" . projectile-command-map)
  :custom ((projectile-auto-discover nil)
           (projectile-enable-caching nil)
           (projectile-git-fd-args "-H -0 -E .git -tf")
           (projectile-ignored-projects '("~/" "~/.pyenv/"))
           (projectile-indexing-method 'alien)
           (projectile-mode-line-function
            (lambda ()
              (format " [%s]" (projectile-project-name))))
           (projectile-project-root-functions
            '(projectile-root-local
              projectile-root-bottom-up
              projectile-root-top-down
              projectile-root-top-down-recurring))
           (projectile-project-search-path
            `(,(ok-file-expand-user-emacs-file "straight" "repos/")
              (,(expand-file-name "github.com/" (getenv "HOME")) . 2))))
  :ensure-system-package ((ag . "sudo apt install -y silversearcher-ag")
                          (fdfind . "sudo apt install -y fd-find"))
  :config
  (projectile-mode +1)
  (pcase-dolist
      (`(,key ,doc) '(("C-x p" "projectile")
                      ("C-x p 4" "projectile-other-window")
                      ("C-x p 5" "projectile-other-frame")
                      ("C-x p s" "projectile-search")
                      ("C-x p c" "projectile-command")
                      ("C-x p x" "projectile-run")
                      ("C-x p x 4" "projectile-run-other-window")))
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements key doc))))

(provide 'subsys-projectile)
;;; subsys-projectile.el ends here
