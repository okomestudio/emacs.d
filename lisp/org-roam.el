;;; org-roam.el --- org-roam  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Org Roam initialization.
;;
;;; Code:

(require 'ok)

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
         ("C-c C-q" . org-roam-ok-tag-add-or-remove)
         ("C-c a" . org-roam-ok-alias-add-or-remove)
         ("C-c r f" . org-roam-ok-ref-find)
         ("C-M-i" . completion-at-point))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :custom ((org-roam-completion-everywhere nil)
           (org-roam-database-connector 'sqlite-builtin)
           (org-roam-db-location (no-littering-expand-var-file-name
                                  "org-roam/roam/.roam.db"))
           (org-roam-directory (no-littering-expand-var-file-name
                                "org-roam/roam/"))
           (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
           (org-roam-mode-sections (list #'org-roam-backlinks-section
                                         #'org-roam-reflinks-section
                                         ;; #'org-roam-unlinked-references-section
                                         #'orp-ok-ja-unlinked-references-section
                                         ))
           (org-roam-node-display-template (concat "​​​​​${orp-title:*} "
                                                   (propertize "${orp-tags}"
                                                               'face 'org-tag)
                                                   " ${orp-timestamp:10}")))
  :preface
  (ok-safe-local-variable-add orb-preformat-keywords listp
                              org-roam-capture-templates listp
                              org-roam-dailies-capture-templates listp
                              org-roam-dailies-directory stringp
                              org-roam-db-location stringp
                              org-roam-directory stringp
                              org-roam-mode-sections listp
                              org-roam-ui-port integerp

                              ;; For browsing source code
                              elisp-lint-ignored-validators listp
                              elisp-lint-indent-specs listp)

  :config
  (setopt find-file-visit-truename t)  ; see "5.3 Setting up Org-roam"

  (defun org-roam-ok-tag-add-or-remove (&optional arg)
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

  (defun org-roam-ok-alias-add-or-remove (&optional arg)
    "Add an Org Roam alias.
When called with the `\\[universal-argument]' prefix argument,
the alias removal action gets triggered."
    (interactive "P")
    (call-interactively (pcase arg
                          ('(4) #'org-roam-alias-remove)
                          (_ #'org-roam-alias-add))))

  (defun org-roam-ok-ref-find (&optional arg)
    "Call the enhanced version of `org-roam-ref-find'.
If the point is on a link and it is a cite link, then
`org-roam-ref-find' is given the citekey as the initial string.
Otherwise, it is the same as the vanilla version of
`org-roam-ref-find'."
    (interactive)
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (path (org-element-property :path link))
           (ref (cond
                 ((string= type "cite")
                  (string-trim-left path "&"))
                 (t ""))))
      (org-roam-ref-find ref)))

  (require 'org-roam-dailies)

  (org-roam-db-sync)
  (org-roam-db-autosync-mode 1)
  (org-roam-gt-mode 1)
  (org-roam-plugin-ok-mode 1))

(use-package ok-plural
  :straight (ok-plural :host github :repo "okomestudio/ok-plural.el"))

(use-package org-roam-plugin-ok
  :straight (:host github :repo "okomestudio/org-roam-plugin-ok")
  :custom ((orp-ok-node-use-cache-in-memory t))
  :init (org-roam-plugin-ok-on-idle-init-setup))

(use-package org-roam-gt
  :straight (:host github :repo "dmgerman/org-roam-gt"
                   :fork (:host github
                                :repo "okomestudio/org-roam-gt" :branch "ok"))
  :demand t
  :config
  ;; String format version
  ;; (setq org-roam-gt-node-display-template
  ;;       (concat "​​​​​${orp-title:*} "
  ;;               (propertize "${orp-tags}" 'face 'org-tag)
  ;;               " ${orp-timestamp:10}"))

  ;; Function version
  (setq org-roam-gt-node-display-template
        (lambda (node)
          (let* ((title (orp-ok-node--title node))
                 (tags (or (orp-ok-node--tags node) ""))
                 (scale 1.7)
                 (total-width (frame-width))
                 (timestamp-width 11)
                 (title-and-tags-width (- total-width timestamp-width))
                 (tags-width (1+ (length tags)))
                 (max-title-width (- title-and-tags-width tags-width))
                 (title-width (ok-string-multibyte-string-width title scale)))
            (if (>= title-width max-title-width)
                (put-text-property (length (ok-string-multibyte-substring
                                            title 0 max-title-width scale))
                                   (length title)
                                   'invisible t title)
              (setq title (concat title
                                  (make-string (- max-title-width
                                                  title-width) ?\ ))))
            (concat title
                    " " (propertize tags 'face 'org-tag)
                    " " (orp-ok-node--timestamp node))))))

(use-package org-roam-ui
  :after org-roam
  :custom ((org-roam-ui-follow t)
           (org-roam-ui-sync-theme t)
           (org-roam-ui-update-on-save t)))

(use-package org-roam-bibtex
  :after org-roam
  :custom ((orb-insert-link-description "${author-abbrev} ${date}")
           (orb-roam-ref-format 'org-ref-v3))
  :config
  (require 'org-ref))  ; optional, if using Org-ref v2 or v3 citation links

(use-package org-ref
  ;; For citations, cross-references, bibliographies.
  :custom (bibtex-completion-pdf-field "file")
  :preface (put 'bibtex-completion-bibliography 'safe-local-variable #'listp))

(use-package org-roam-timestamps
  :after org-roam
  :custom ((org-roam-timestamps-minimum-gap 86400)
           (org-roam-timestamps-remember-timestamps t)
           (org-roam-timestamps-timestamp-parent-file nil))
  :config (org-roam-timestamps-mode))

;;; org-roam.el ends here
