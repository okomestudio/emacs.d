;;; subsys-org-roam.el --- Org Roam Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Org Roam subsystem.
;;
;;; Code:

(require 'ok)

(use-package org-roam
  :straight (org-roam
             :host github
             :repo "org-roam/org-roam"  ; official version

             ;; Use the fork for development:
             ;; :repo "okomestudio/org-roam"
             ;; :fork "okomestudio"
             ;; :branch "refactor-org-roam-unlinked-references-section"

             :files (:defaults "extensions/*"))
  :bind (("C-c n c" . org-roam-ok-capture-create-from-ref)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ("C-c C-q" . org-roam-ok-node-tag-add-or-remove)
         ("C-c a" . org-roam-ok-node-alias-add-or-remove)
         ("C-c r f" . org-roam-ok-node-ref-find))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :custom ((org-roam-completion-everywhere nil)
           (org-roam-database-connector 'sqlite-builtin)
           (org-roam-db-location (ok-file-expand-var "org-roam/roam/.roam.db"))
           (org-roam-directory (ok-file-expand-var "org-roam/roam/"))
           (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
           (org-roam-mode-sections (list #'org-roam-backlinks-section
                                         #'org-roam-reflinks-section
                                         ;; #'org-roam-unlinked-references-section
                                         #'org-roam-ok-ja-unlinked-references-section
                                         ))
           ;; NOTE: When `org-roam-gt' is in use, see
           ;; `org-roam-gt-node-display-template' in
           ;; `orp-ok-node-gt.el' for the function version:
           (org-roam-node-display-template (concat "${orp-title:*} "
                                                   (propertize "${orp-tags}"
                                                               'face 'org-tag)
                                                   " ${orp-timestamp:10}")))
  :config
  (org-roam-ok-enhance)
  (org-roam-db-sync)
  (org-roam-db-autosync-mode 1))

(use-package ok-plural
  :straight (ok-plural :host github :repo "okomestudio/ok-plural.el"))

(use-package org-roam-fz
  :straight (org-roam-fz :host github :repo "okomestudio/org-roam-fz")
  :hook (org-mode . org-roam-fz-mode))

(use-package org-roam-gt
  ;; See github.com/org-roam/org-roam/issues/2474
  :straight (org-roam-gt :host github
                         :repo "dmgerman/org-roam-gt"
                         :fork (:host github
                                      :repo "okomestudio/org-roam-gt"
                                      :branch "ok")))

(use-package org-roam-ok
  :straight (org-roam-ok :host github
                         :repo "okomestudio/org-roam-ok"
                         :files (:defaults "extensions/*"))
  :custom ((org-roam-ok-node-use-cache-in-memory t)
           (org-roam-ok-node-gt-use-cache-in-memory t)
           (org-roam-ok-on-idle-delay 3))
  :init
  (load (ok-file-expand-etc "org-roam/init"))
  (org-roam-ok-on-idle-init-setup))

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
  :custom (bibtex-completion-pdf-field "file"))

(use-package org-roam-timestamps
  :after org-roam
  :custom ((org-roam-timestamps-minimum-gap 86400)
           (org-roam-timestamps-remember-timestamps t)
           (org-roam-timestamps-timestamp-parent-file nil))
  :config (org-roam-timestamps-mode))

(provide 'subsys-org-roam)
;;; subsys-org-roam.el ends here
