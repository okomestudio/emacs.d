;;; init-web-browsing.el --- Web browsing  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Packages for web browsing.
;;
;;; Code:


(use-package browse-url
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (browse-url-generic-program "xdg-open")
  (browse-url-handlers '(("localhost" . browse-url-generic))))


(require 'init-anki)
(require 'init-elfeed)
(require 'init-eww)
(require 'init-gnus)
(require 'init-osm)
(require 'init-slack)


(provide 'init-web-browsing)
;;; init-web-browsing.el ends here
