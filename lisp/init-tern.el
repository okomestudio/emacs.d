;;; init-tern.el --- Tern  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package tern
  :ensure nil

  :custom
  (tern-command '("tern" "--no-port-file"))

  :ensure-system-package
  (tern . "npm install -g tern")

  :init
  (require 'okutil)
  (okutil-ensure-file-from-github "ternjs/tern/master/emacs/tern.el"))


(provide 'init-tern)
;;; init-tern.el ends here
