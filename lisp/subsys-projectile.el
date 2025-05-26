;;; subsys-projectile.el --- Projectile Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the projectile subsystem.
;;
;;; Code:

(require 'dash)
(require 'ok)

(use-package projectile
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
  :hook ((after-init . projectile-mode)
         (projectile-mode . projectile-ok-set-safe-local-variable-directories))
  :ensure-system-package ((ag . "sudo apt install -y silversearcher-ag")
                          (fdfind . "sudo apt install -y fd-find"))
  :config
  ;; Ensure all the projectile overrides happen after `project' is
  ;; loaded.
  (require 'project)

  ;; NOTE(2025-05-19): The following remap shadows all pre-existing
  ;; `project-' commands originally mapped to `C-x p'. Rebind each
  ;; individually if necessary.
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

  (which-key-add-key-based-replacements
    "C-x p" "projectile"
    "C-x p 4" "projectile-other-window"
    "C-x p 5" "projectile-other-frame"
    "C-x p s" "projectile-search"
    "C-x p c" "projectile-command"
    "C-x p x" "projectile-run"
    "C-x p x 4" "projectile-run-other-window")

  ;; Dynamical update of `safe-local-variable-directories'

  (cl-defun projectile-ok-set-safe-local-variable-directories ()
    "Update `safe-local-variable-directories' with projectile.
To avoid being prompted for safeness, a symbol `sym' needs to be in
`safe-local-variable', e.g.,

  (put 'sym 'safe-local-variable #'integerp)
  (ok-safe-local-variable-add sym integerp)

This function uses `safe-local-variable-directories' introduced in Emacs
30.1 to declare a local directory tree as safe."
    (interactive)
    (let* ((projectile-current-project-on-switch 'keep)
           (ds (projectile-relevant-known-projects))
           (symlink-tree (getenv "SYMLINKTO_TREE")))
      ;; When `SYMLINKTO_TREE' is defined, create a `symlinkto' path
      ;; for each known project directory.
      (when symlink-tree
        (setq ds (append ds (--map (string-replace "~" symlink-tree it) ds))))
      (setopt safe-local-variable-directories (delete-dups ds))))

  (cl-defun projectile-ok--add-to-safe-local-variable-directories ()
    "Add the current project root to `safe-local-variable-directories'."
    (when (projectile-project-root)
      (add-to-list 'safe-local-variable-directories
                   (abbreviate-file-name (projectile-project-root)))))

  (add-hook 'hack-local-variables-hook
            #'projectile-ok--add-to-safe-local-variable-directories))

(provide 'subsys-projectile)
;;; subsys-projectile.el ends here
