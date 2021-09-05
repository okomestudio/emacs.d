;;; init-google-translate.el --- Google-Translate  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Visual Popup User Interface
(use-package popup)

;; Emacs interface to Google Translate
;; -----------------------------------
;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :after popup

  :bind
  (("C-c t" . 'google-translate-smooth-translate))

  :custom
  (google-translate-translation-directions-alist '(("ja" . "en") ("en" . "ja")))

  :config
  (require 'google-translate-smooth-ui))

(provide 'init-google-translate)
;;; init-google-translate.el ends here
