;;; browse-url.el --- browse-url  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; `browse-url' configuration.
;;
;;; Code:

(use-package browse-url
  :straight nil
  :custom ((browse-url-browser-function 'browse-url-ok-browser-function)
           (browse-url-generic-program "xdg-open")
           (browse-url-handlers '(("localhost" . browse-url-generic))))
  :config
  (defun browse-url-ok-browser-function (url &optional arg)
    (interactive "sURL: \nP")
    (pcase arg
      ('(4) (browse-url-default-browser url))
      (_ (eww-browse-url url)))))

;;; brose-url.el ends here
