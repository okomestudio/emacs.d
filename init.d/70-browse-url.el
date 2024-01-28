;;; 70-browse-url.el --- browse-url  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize browser-url.
;;
;;; Code:

(use-package browse-url
  :after (eww)
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (browse-url-generic-program "xdg-open")
  (browse-url-handlers '(("localhost" . browse-url-generic))))

;;; 70-brose-url.el ends here
