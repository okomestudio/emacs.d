;;; restclient.el --- restclient.el  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure restclient and related utilities.
;;
;;; Code:

(use-package restclient ;; derives from fundamental-mode
  ;; HTTP REST client.
  )

(use-package company-restclient
  :disabled)

(use-package ob-restclient
  ;; An org-mode extension to restclient.el
  :disabled)

;;; restclient.el ends here
