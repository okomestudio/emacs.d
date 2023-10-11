;;; init-shell.el --- Shell  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :straight nil

  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)

  :hook
  (sh-mode . flymake-mode)

  :mode
  ("\\.sh\\'"
   "bash_*"
   "bashrc\\'")

  ;; NOTE: Commented out in case we need the lines later.
  ;;
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))
  ;; (add-to-list 'interpreter-mode-alist '("bats" . sh-mode))
  )

(use-package flymake
  :ensure-system-package
  (shellcheck . "sudo apt install -y shellcheck"))

(use-package bats-mode)

(use-package ansi-color
  :hook
  (compilation-filter . ts/colorize-buffer)

  :config
  (defun ts/colorize-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(provide 'init-shell)
;;; init-shell.el ends here
