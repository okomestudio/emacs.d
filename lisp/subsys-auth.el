;;; subsys-auth.el --- Auth Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the auth subsystem.
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

(provide 'subsys-auth)
;;; subsys-auth.el ends here