;;; 16-org-roam.el --- org-roam  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize and configure Org Roam and the related utilities.
;;
;;; Code:

(use-package org-roam
  :straight (;; Pull from GitHub:
             :host github

             ;; Use the official version:
             :repo "org-roam/org-roam"

             ;; Use the fork for development:
             ;; :repo "okomestudio/org-roam"
             ;; :fork "okomestudio"
             ;; :branch "refactor-org-roam-unlinked-references-section"

             :files (:defaults "extensions/*"))

  :bind (("C-c n c" . (lambda () (interactive) (org-capture nil "f")))
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         :map org-mode-map
         ("C-c C-q" . ok-org-roam-tag-add-or-remove)
         ("C-c a" . ok-org-roam-alias-add-or-remove)
         ("C-c r f" . ok-org-roam-ref-find)
         ("C-M-i" . completion-at-point))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)

  :custom
  (org-roam-completion-everywhere nil)
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %?\n<%<%Y-%m-%d %a %H:%M>>"
                                         :target
                                         (file+head "%<%Y-%m-%d>.org"
                                                    "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-dailies-directory "journal/")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-location (no-littering-expand-var-file-name "org-roam/roam/.roam.db"))
  (org-roam-directory (no-littering-expand-var-file-name "org-roam/roam/"))
  (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
  (org-roam-mode-sections (list #'org-roam-backlinks-section
                                #'org-roam-reflinks-section
                                ;; #'org-roam-unlinked-references-section
                                #'orp-ok-ja-unlinked-references-section
                                ))
  (org-roam-node-display-template (concat "​​​​​${orp-title:*} "
                                          (propertize "${orp-tags}"
                                                      'face 'org-tag)
                                          " ${orp-timestamp:10}"))

  :preface
  (ok-safe-local-variable-add orb-preformat-keywords listp
                              org-roam-capture-templates listp
                              org-roam-dailies-capture-templates listp
                              org-roam-db-location stringp
                              org-roam-directory stringp
                              org-roam-mode-sections listp
                              org-roam-ui-port integerp)

  :config
  (use-package org-roam-plugin-ok
    :straight (:host github :repo "okomestudio/org-roam-plugin-ok")
    :demand t)

  (setopt find-file-visit-truename t) ;; See 5.3 Setting up Org-roam

  (defun ok-org-roam-tag-add-or-remove (&optional arg)
    "Add an Org filetags or heading tag.

When called with the `\\[universal-argument]'
`\\[universal-argument]' `\\[universal-argument]' prefix
argument, the tag removal action gets triggered."
    (interactive "P")
    (call-interactively
     (if (not (org-before-first-heading-p))
         ;; For non-filetags tags, use `org-set-tags-command' for both
         ;; addition and removal:
         #'org-set-tags-command
       (pcase arg
         ('(64) #'org-roam-tag-remove)
         (_ #'org-roam-tag-add)))))

  (defun ok-org-roam-alias-add-or-remove (&optional arg)
    "Add an Org Roam alias.

When called with the `\\[universal-argument]' prefix argument,
the alias removal action gets triggered."
    (interactive "P")
    (call-interactively (pcase arg
                          ('(4) #'org-roam-alias-remove)
                          (_ #'org-roam-alias-add))))

  (defun ok-org-roam-ref-find (&optional arg)
    "Call the enhanced version of `org-roam-ref-find'.

If the point is on a link and it is a cite link, then
`org-roam-ref-find' is given the citekey as the initial string.
Otherwise, it is the same as the vanilla version of
`org-roam-ref-find'."
    (interactive)
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (path (org-element-property :path link))
           (ref (if (string= type "cite")
                    (string-trim-left path "&")
                  "")))
      (org-roam-ref-find ref)))

  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))


(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-follow t)
  (org-roam-ui-sync-theme t)
  (org-roam-ui-update-on-save t))


(use-package org-roam-bibtex
  :after org-roam
  :custom
  (orb-insert-link-description "${author-abbrev} ${date}")
  (orb-roam-ref-format 'org-ref-v3)

  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links


(use-package org-ref
  ;; For citations, cross-references, bibliographies.
  :custom (bibtex-completion-pdf-field "file")
  :preface (put 'bibtex-completion-bibliography 'safe-local-variable #'listp))


(use-package org-roam-timestamps
  :after org-roam
  :config (org-roam-timestamps-mode)
  :custom
  (org-roam-timestamps-minimum-gap 86400)
  (org-roam-timestamps-remember-timestamps t)
  (org-roam-timestamps-timestamp-parent-file nil))

;; Local Variables:
;; nameless-aliases: (("" . "ok-org-roam"))
;; End:
;;; 16-org-roam.el ends here
