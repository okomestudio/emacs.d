;;; init-google-translate.el --- Google-Translate  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package google-translate
  :bind
  (("C-c t" . 'google-translate-at-point)
   ("C-c T" . 'google-translate-query-translate))

  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  :custom
  ((google-translate-backend-method 'curl)
   (google-translate-default-source-language "auto")
   (google-translate-default-target-language "ja")))

(provide 'init-google-translate)
;;; init-google-translate.el ends here
