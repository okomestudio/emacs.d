;;; init-keychain.el --- KEYCHAIN  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package keychain-environment
  :ensure nil
  :init
  (ensure-file-from-github "tarsius/keychain-environment/master/keychain-environment.el")
  (require 'keychain-environment)
  (keychain-refresh-environment))

(provide 'init-keychain)
;;; init-keychain.el ends here
