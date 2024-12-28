;;; subsys-http.el --- HTTP Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the HTTP subsystem.
;;
;;; Code:

;;; Client for HTTP request/response inspection

;; restclient

(use-package restclient ;; derives from fundamental-mode
  ;; HTTP REST client.
  )

(use-package company-restclient
  :disabled)

(use-package ob-restclient
  ;; An org-mode extension to restclient.el
  :disabled)

;; Other client options:
;;
;;   1. verb (github.com/federicotdn/verb)
;;   2. plz-see.el (github.com/astoff/plz-see.el)
;;   3. ob-http (github.com/zweifisch/ob-http)
;;   4. hurl-mode (github.com/JasZhe/hurl-mode)
;;   5. httprepl.el (github.com/gregsexton/httprepl.el)

(provide 'subsys-http)
;;; subsys-http.el ends here
