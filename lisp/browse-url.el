;;; browse-url.el --- browse-url  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; `browse-url' configuration.
;;
;;; Code:

(use-package browse-url
  :straight nil
  :after (eww)
  :custom ((browse-url-browser-function 'ok-browse-url-browser-function)
           (browse-url-generic-program "xdg-open")
           (browse-url-handlers '(("localhost" . browse-url-generic))))
  :config
  (defun ok-browse-url-browser-function (url &optional arg)
    (interactive "sURL: \nP")
    (pcase arg
      ('(4) (browse-url-default-browser url))
      (_ (eww-browse-url url)))))

;;; brose-url.el ends here
