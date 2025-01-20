;;; subsys-zotero.el --- Zotero Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Zotero subsystem.
;;
;;; Code:

(use-package zotero
  ;; Interface to the Zotero Web API v3.
  :commands (zotero-browser)
  :init (require 'zotero-browser)
  :config
  (let ((auth (car (auth-source-search :host "api.zotero.org"))))
    (setq zotero-auth-token
          (zotero-auth-token-create :token (plist-get auth :key)
                                    :token-secret (plist-get auth :key)
                                    :userid (plist-get auth :userid)
                                    :username (plist-get auth :username)))))

(provide 'subsys-zotero)
;;; subsys-zotero.el ends here
