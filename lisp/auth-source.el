;;; auth-source.el --- auth-source  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The auth-source configuration.
;;
;;; Code:

(use-package auth-source
  :straight nil
  :custom ((auth-sources `(,(no-littering-expand-etc-file-name
                             "auth-source/.authinfo.gpg")))
           (authinfo-hidden (format "\\(%s\\)"
                                    (mapconcat 'identity
                                               '("api_key"
                                                 "api_token"
                                                 "authkey"
                                                 "cookie"
                                                 "secret"
                                                 "passphrase"
                                                 "password")
                                               "\\|")))))

;;; auth-source.el ends here
