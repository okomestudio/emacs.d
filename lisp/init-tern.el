;;; init-tern.el --- Tern  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tern
  :ensure nil

  :custom
  (tern-command '("tern" "--no-port-file"))

  :ensure-system-package
  ((tern . "sudo npm install -g tern"))

  :init
  (ensure-file-from-github "ternjs/tern/master/emacs/tern.el"))

(provide 'init-tern)
;;; init-tern.el ends here
