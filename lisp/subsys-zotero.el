;;; subsys-zotero.el --- Zotero  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Zotero subsystem.
;;
;;; Code:

(use-package zotxt
  ;; The zotxt integration for Emacs.
  ;;
  ;; The local Zotero installation needs Better BibTeX
  ;; (https://retorque.re/zotero-better-bibtex/) and zotxt
  ;; (https://github.com/egh/zotxt) addons installed.
  ;;
  ;; NOTE: Set `zotxt-default-bibliography-style' to one of cite styles found in
  ;; Style Manager in Zotero. (The zotxt default is "chicago-note-bibliography.)
  ;; See https://github.com/egh/zotxt-emacs/issues/50.
  :bind ( ("C-c r o" . zotxt-citekey-select-item-at-point) )
  :custom (zotxt-default-bibliography-style "chicago-shortened-notes-bibliography")
  :hook ((org-mode . org-zotxt-mode)
         (org-mode . zotxt-citekey-mode))
  :config
  (defun zotxt-citekey-at-point-match--ad (fun &rest _)
    "Advise `zotxt-citekey-at-point-match' to match Org elements.

The citekey is of form \"&citekey\".

With this advice, a citekey can be extracted from an Org link or a flat text
starting from \"&\"."
    (if (derived-mode-p 'org-mode)
        (cond
         ((org-in-regexp org-link-any-re)
          (save-excursion
            (goto-char (match-beginning 0))
            (let ((zotxt-citekey-regex (concat "\\(?:\\[\\[\\)?"
                                               "cite:&\\([[:alnum:].]+\\)"
                                               "\\(?:\\]\\[[^]]+\\]\\]\\)?")))
              (apply fun _))))
         (t (apply fun _)))
      (apply fun _)))

  (advice-add #'zotxt-citekey-at-point-match :around
              #'zotxt-citekey-at-point-match--ad))

(use-package zotero
  ;; Interface to the Zotero Web API v3.
  :disabled
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
