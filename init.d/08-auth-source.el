;;; 08-auth-source.el --- auth-source  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package auth-source
  :defer t
  :straight nil

  :custom
  (auth-sources `((:source
                   ,(expand-file-name "secrets/.authinfo.gpg"
                                      user-emacs-directory))))
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

;;; 08-auth-source.el ends here
