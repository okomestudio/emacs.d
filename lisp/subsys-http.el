;;; subsys-http.el --- HTTP Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the HTTP subsystem.
;;
;;; Code:

;;; HTTP Inspection Tools
;;
;; `restclient' has been in Emacs orphanage. Other client options include:
;;
;;   1. verb (github.com/federicotdn/verb)
;;   2. plz-see.el (github.com/astoff/plz-see.el)
;;   3. ob-http (github.com/zweifisch/ob-http)
;;   4. hurl-mode (github.com/JasZhe/hurl-mode)
;;   5. httprepl.el (github.com/gregsexton/httprepl.el)

(use-package restclient
  ;; HTTP REST client.
  :mode ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :disabled)

(use-package ob-restclient
  ;; An org-mode extension to restclient.el
  :disabled)

(provide 'subsys-http)
;;; subsys-http.el ends here
