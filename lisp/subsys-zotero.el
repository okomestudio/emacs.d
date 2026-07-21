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
  :config
  (defun zotxt-citekey-at-point-match--ad (fun &rest _r)
    "Advise `zotxt-citekey-at-point-match' to match wider range of citekeys.
The purpose is to match org-ref version 2 style citekeys, as well as to match
citekeys in Org links more consistently."
    (if (derived-mode-p 'org-mode)
        (let ((zotxt-citekey-regex "[@{&]\\([[:alnum:].-]+\\)"))
          (if-let* ((m (org-in-regexp org-link-any-re))
                    (zotxt-citekey-regex
                     (concat "\\(?:\\[\\(?:\\[\\)?\\)?"
                             (regexp-opt (org-link-types) nil) ":"
                             zotxt-citekey-regex)))
              (let ((beg (car m)))
                (goto-char beg)
                (apply fun _r))
            (apply fun _r)))
      (apply fun _r)))

  (advice-add #'zotxt-citekey-at-point-match :around
              #'zotxt-citekey-at-point-match--ad)

  :hook ((org-mode . org-zotxt-mode)
         (org-mode . zotxt-citekey-mode)))

(use-package zotero
  ;; Interface to the Zotero Web API v3.
  :config
  (load-private-init "zotero" t))

(use-package ndl-search
  :after zotero
  :commands ndl-search-zotero-create-item)

(provide 'subsys-zotero)
;;; subsys-zotero.el ends here
