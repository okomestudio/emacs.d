;;; init-google-translate.el --- Google-Translate  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Emacs interface to Google Translate
;; -----------------------------------
;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :after popup

  :bind
  (("C-c t" . 'google-translate-smooth-translate))

  :config
  (require 'google-translate)
  (require 'google-translate-smooth-ui)

  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  (setq google-translate-backend-method 'curl
        google-translate-output-destination nil
        google-translate-translation-directions-alist '(("ja" . "en") ("en" . "ja")) ))

(use-package popup)

(provide 'init-google-translate)
;;; init-google-translate.el ends here
