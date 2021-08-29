;;; init-shell.el --- Shell  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-mode
  :ensure nil

  :custom
  ((sh-basic-offset 4)
   (sh-indentation 4))

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

(provide 'init-shell)
;;; init-shell.el ends here
