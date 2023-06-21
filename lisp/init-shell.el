;;; init-shell.el --- Shell  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-mode
  :straight nil

  :custom
  (sh-basic-offset 4)
  (sh-indentation 4)

  :mode
  ("\\.sh\\'"
   "bash_*"
   "bashrc\\'"))

(use-package bats-mode)

(use-package ansi-color
  :hook
  (compilation-filter . ts/colorize-buffer)

  :config
  (defun ts/colorize-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(use-package flymake-shellcheck
  :disabled
  :if (executable-find "shellcheck")
  :commands flymake-shellcheck-load

  :ensure-system-package
  (shellcheck . "sudo apt install -y shellcheck")

  :hook
  ((sh-mode) . flymake-shellcheck-load)

  :init
  (setq sh-basic-offset 4
        sh-indentation 4)
  (add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))
  (add-to-list 'interpreter-mode-alist '("bats" . sh-mode)))

(provide 'init-shell)
;;; init-shell.el ends here
