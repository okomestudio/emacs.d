;;; init-shell.el --- Shell  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package bats-mode)

(use-package sh-mode
  :custom
  ((sh-basic-offset 4)
   (sh-indentation 4))
  :ensure nil
  :mode ("\\.sh\\'"
         "bash_*"
         "bashrc\\'"))

(provide 'init-shell)
;;; init-shell.el ends here
