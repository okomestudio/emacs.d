;;; init-auth-source.el --- auth-source  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package auth-source
  :straight nil

  :custom
  (auth-sources '((:source "~/.config/emacs/secrets/.authinfo.gpg")))
  (authinfo-hidden (concat
                    "\\("
                    (mapconcat
                     'identity
                     '("api_token"
                       "authkey"
                       "cookie"
                       "secret"
                       "passphrase"
                       "password")
                     "\\|")
                    "\\)")))


(provide 'init-auth-source)
;;; init-auth-source.el ends here
