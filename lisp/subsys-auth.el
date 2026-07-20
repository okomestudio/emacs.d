;;; subsys-auth.el --- Auth  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the auth subsystem.
;;
;;; Code:

(use-package auth-source
  :custom ((auth-sources (list (fs-emacs-etc "auth-source/.authinfo.gpg"))))
  :config
  (setopt authinfo-hidden
          (regexp-opt '("api_key" "api_token" "authkey"
                        "cookie"
                        "key"
                        "secret"
                        "passphrase" "password")))

  ;; (font-lock-add-keywords 'authinfo-mode ... doesn't work due to a timing
  ;; (?) issue. That's the reason for using the mode init hook below.

  (defun authinfo-mode--init ()
    (font-lock-add-keywords
     nil
     `((,(regexp-opt '("api_key" "api_token"
                       "cookie"
                       "key" "keyname"
                       "userid" "username")
                     'symbols)
        (1 font-lock-comment-delimiter-face keep))))
    (font-lock-flush))

  :hook (authinfo-mode . authinfo-mode--init))

(provide 'subsys-auth)
;;; subsys-auth.el ends here
