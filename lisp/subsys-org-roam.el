;;; subsys-org-roam.el --- Org Roam  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Org Roam subsystem.
;;
;;; Code:

(require 'ok)

(use-package org-roam
  :straight (org-roam :host github
                      :repo "org-roam/org-roam"
                      :fork (:branch "fix-autosync-to-vc-delete-file"))
  :bind ( ("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n l" . org-roam-buffer-toggle)
          ("C-c r c" . org-roam-ok-capture-create-from-ref)
          ("C-c r f" . org-roam-ok-node-ref-find)
          ("C-c r i" . org-ref-insert-link)

          :map org-mode-map
          ("C-M-i" . completion-at-point)
          ("C-c C-q" . org-roam-ok-node-tag-add-or-remove)
          ("C-c a" . org-roam-ok-node-alias-add-or-remove) )
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :custom ((org-roam-completion-everywhere nil)
           (org-roam-database-connector 'sqlite-builtin)
           (org-roam-db-location (ok-file-expand-var "org-roam/roam/.roam.db"))
           (org-roam-directory (ok-file-expand-var "org-roam/roam/"))
           (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
           (org-roam-node-display-template "${title}"))
  :init
  (which-key-add-key-based-replacements
    "C-c n" "org-roam"
    "C-c n d" "org-roam-dailies"
    "C-c r" "org-ref")

  :config
  (require 'org-roam-cjk-ja)
  (org-roam-ok-enhance)
  (org-roam-db-sync)
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :after org-roam
  :custom ((org-roam-ui-follow t)
           (org-roam-ui-sync-theme t)
           (org-roam-ui-update-on-save t)))

(use-package org-roam-node-display-cache
  :straight (org-roam-node-display-cache
             :host github
             :repo "okomestudio/org-roam-node-display-cache")
  :hook (org-roam-ok-mode . org-roam-node-display-cache-mode)
  :init
  (defun org-roam-node-display-cache--ensure-desktop ()
    "Ensure the registration of global variable for desktop save."
    (require 'org-roam-node-display-cache)
    (add-to-list 'desktop-globals-to-save 'org-roam-node-display-cache--cache)

    ;; Hash table cannot be serialized easily to persist in a file.
    ;; The functions thus converts it to an alist in the de/ser layer.
    (add-to-list 'ok-desktop-global-var-serdes-funs
                 (list 'org-roam-node-display-cache--cache
                       (lambda (ht)
                         (let (alist)
                           (maphash (lambda (k v) (push (cons k v) alist)) ht)
                           alist))
                       (lambda (alist)
                         (if (hash-table-p alist)
                             alist
                           (let ((ht (make-hash-table :test 'equal)))
                             (dolist (entry alist)
                               (puthash (car entry) (cdr entry) ht))
                             ht))))))
  (add-hook 'desktop-save-hook
            #'org-roam-node-display-cache--ensure-desktop)
  (add-hook 'ok-desktop-before-read-hook
            #'org-roam-node-display-cache--ensure-desktop))

;;; OK-Specific Enhancement

(use-package org-roam-timestamps
  ;; Add timestamps to `org-roam' notes.
  :after org-roam
  :custom ((org-roam-timestamps-minimum-gap 86400)
           (org-roam-timestamps-remember-timestamps t)
           (org-roam-timestamps-timestamp-parent-file nil))
  :config (org-roam-timestamps-mode))

(use-package org-roam-ok
  :straight (org-roam-ok :host github
                         :repo "okomestudio/org-roam-ok"
                         :files (:defaults "extensions/*"))
  :custom ((org-roam-ok-node-use-cache-in-memory t)
           (org-roam-ok-on-idle-delay nil))
  :init
  (load (ok-file-expand-etc "org-roam/init"))
  (org-roam-ok-on-idle-init-setup))

(use-package adaptive-wrap)

(use-package ok-plural
  :straight (ok-plural :host github :repo "okomestudio/ok-plural.el"))

(use-package org-roam-cjk
  :straight (org-roam-cjk :host github
                          :repo "okomestudio/org-roam-cjk"
                          :files (:defaults "extensions/*"))
  :custom ((org-roam-mode-sections
            (list #'org-roam-backlinks-section
                  #'org-roam-reflinks-section
                  #'org-roam-cjk-unlinked-references-section)))
  :config
  (require 'adaptive-wrap)
  (require 'ok-plural)

  (setopt org-roam-cjk-unlinked-references-ignore-lines
          '("PYTHONDONTWRITEBYTECODE=1 "
            "begin_src.+"
            "export_hugo_bundle:.+"
            "filetags:.+"
            "hugo_bundle:.+"
            "hugo_tags:.+"
            "parent:.+"
            "property:.+"
            "transclude:.+"))

  (defun org-roam-ja--pluralize (title)
    "Pluralize noun(s) in TITLE."
    (let* ((tokens (string-split title " "))
           (pluralized-tokens (mapcar (lambda (token)
                                        (ok-plural-pluralize token))
                                      tokens)))
      ;; TODO: Expand the action based on permutations, not just the last token
      (string-join (append (butlast tokens)
                           `(,(cond
                               ((car (last pluralized-tokens))
                                (car (last pluralized-tokens)))
                               (t (car (last tokens))))))
                   " ")))

  (defun org-roam-ja--pluralize-titles (fun titles)
    "Expand TITLES with their plural forms."
    (apply fun `(,(flatten-list
                   (mapcar (lambda (w)
                             (let ((p (org-roam-ja--pluralize w)))
                               (if p `(,w ,p) `(,w))))
                           titles)))))

  (advice-add #'org-roam-ja-unlinked-references-section :around
              #'org-roam-ja--pluralize-titles))

;;; Bibliographic Reference Management

(use-package org-roam-bibtex
  :after org-roam
  :custom ((orb-insert-link-description "${author-abbrev} ${date}")
           (orb-roam-ref-format 'org-ref-v3))
  :config
  (require 'org-ref))  ; optional, if using Org-ref v2 or v3 citation links

(use-package org-ref
  ;; For citations, cross-references, bibliographies.
  :custom (bibtex-completion-pdf-field "file"))

;;; Misc.

(use-package org-roam-fz
  ;; Folgezettel utility.
  :straight (org-roam-fz :host github :repo "okomestudio/org-roam-fz")
  :custom (org-roam-ok-node-display-title #'org-roam-fz--display-title)
  :hook (org-mode . org-roam-fz-mode)
  :config
  (defun org-roam-fz--display-title (node)
    "Render NODE title for display."
    (concat (org-roam-ok-node--title node)
            (when-let* ((fid (org-roam-node-fid node)))
              (concat " " fid)))))

(provide 'subsys-org-roam)
;;; subsys-org-roam.el ends here
