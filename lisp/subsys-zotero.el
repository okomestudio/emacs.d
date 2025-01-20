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
                                    :username (plist-get auth :username))))

  (defun zotero-ok-item-search (query)
    "Make a QUERY for an item in Zotero."
    (interactive)
    (let* ((resp (zotero-search-items query))
           (items (zotero-response-data resp))
           (cands (seq-map
                   (lambda (item)
                     (let* ((data (plist-get item :data))
                            (item-type (plist-get data :itemType))
                            (title (plist-get data :title))
                            (creators (plist-get data :creators))
                            (names (seq-map
                                    (lambda (c)
                                      (or (plist-get c :name)
                                          (plist-get c :lastName)))
                                    creators)))
                       (unless (eq item-type "attachment")
                         `(,(format "%16s %32s   %s (%s)"
                                    (concat "[" item-type "]")
                                    (string-join names ", ")
                                    title
                                    (plist-get item :key))
                           ,item))))
                   items))
           (chosen (completing-read "Choose: "
                                    (seq-map (lambda (cand) (car cand))
                                             cands))))
      (cadr (assoc chosen cands)))))

(provide 'subsys-zotero)
;;; subsys-zotero.el ends here
