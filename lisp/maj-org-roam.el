;;; maj-org-roam.el --- Org Roam  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure `org-roam'.
;;
;; Potential tasks:
;;
;; - (2025-10-10) Use github.com/meedstrom/org-roam-async for async DB sync?
;;
;;; Code:

(require 'ok)

(use-package org-roam
  :bind ( ("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n l" . org-roam-buffer-toggle)
          ("C-c r c" . org-roam-ok-capture-create-from-ref)
          ("C-c r f" . org-roam-ref-find)
          ("C-c r i" . bibtex-completion-ok-insert-org-ref-link)
          ("C-c r s" . org-roam-ok-ref-search-buffer)

          :map org-mode-map
          ("C-M-i" . completion-at-point)
          ("C-c C-q" . org-roam-ok-node-tag-add-or-remove)
          ("C-c a" . org-roam-ok-node-alias-add-or-remove) )
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :custom ((org-roam-completion-everywhere nil)
           (org-roam-database-connector 'sqlite-builtin)

           ;; Removed `transclude' to include them in backlinks. (Also, see
           ;; `org-roam-db-extra-links-elements'.)
           (org-roam-db-extra-links-exclude-keys '((node-property "ROAM_REFS")))

           (org-roam-db-location (ok-file-expand-var "org-roam/roam/.roam.db"))
           (org-roam-directory (ok-file-expand-var "org-roam/roam/"))
           (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
           (org-roam-node-display-template "${title}")
           (org-roam-mode-sections
            (list #'org-roam-backlinks-section
                  #'org-roam-reflinks-section
                  #'org-roam-cjk-unlinked-references-section)))
  :init
  (which-key-add-key-based-replacements
    "C-c n" "org-roam"
    "C-c n d" "org-roam-dailies"
    "C-c r" "org-ref")

  (defun org-roam--load-init-lazy ()
    (load (ok-file-expand-etc "org-roam/init")))
  (add-hook 'after-init-hook #'org-roam--load-init-lazy 99)

  :config
  (with-eval-after-load 'savehist
    (setopt savehist-additional-variables
            (append savehist-additional-variables
                    '(org-roam-ref-history))))

  (with-eval-after-load 'vertico-posframe
    (require 'vertico-multiform)
    (add-to-list
     'vertico-multiform-commands
     '(org-roam-node-find
       posframe
       (vertico-count . 32)
       (vertico-posframe-poshandler . posframe-poshandler-frame-center))))

  (require 'org-id-ext)
  (require 'org-roam-cjk-ja)
  (org-roam-ok-mode 1)
  (org-roam-db-sync)
  (org-roam-db-autosync-mode 1))

(use-package consult-org-roam
  :bind ( ("C-c n F" . consult-org-roam-file-find) )
  :config
  ;; (consult-org-roam-mode 1)
  (consult-customize consult-org-roam-file-find
                     :preview-key '(:debounce 0.1 any))

  (with-eval-after-load 'vertico-posframe
    (require 'vertico-multiform)
    ;; NOTE(2025-06-07): posframe often renders vertico incorrectly,
    ;; making it invisible. Watch out for the issue.
    (add-to-list
     'vertico-multiform-commands
     '(consult-org-roam-file-find
       posframe
       (vertico-count . 16)
       (vertico-posframe-poshandler . posframe-poshandler-frame-bottom-center)))))

(use-package org-roam-ui
  :after org-roam
  :custom ((org-roam-ui-follow t)
           (org-roam-ui-sync-theme t)
           (org-roam-ui-update-on-save t)))

(use-package org-roam-node-display-cache
  :hook (org-roam-ok-mode . org-roam-node-display-cache-mode)
  :init
  (defun org-roam-node-display-cache--ensure-desktop ()
    "Ensure the registration of global variable for desktop save."
    (require 'org-roam-node-display-cache)
    (add-to-list 'desktop-globals-to-save 'org-roam-node-display-cache--cache)

    ;; The hash table is serialized to persist in a file.
    (add-to-list 'ok-desktop-global-var-serdes-funs
                 (list 'org-roam-node-display-cache--cache
                       (lambda (ht)
                         (prin1-to-string ht))
                       (lambda (s)
                         (if-let* ((desered (and (stringp s) (read s)))
                                   (_ (hash-table-p desered)))
                             desered
                           (make-hash-table :test 'equal))))))
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
  :config
  (defun org-roam-timestamps-ok--disable (fun &rest _rest)
    "Disable org-roam-timestamps-mode temporarily."
    (org-roam-timestamps-mode -1)
    (apply fun _rest)
    (org-roam-timestamps-mode +1))

  (with-eval-after-load 'org-roam-ok-node
    (advice-add #'org-roam-ok-node-file-save :around
                #'org-roam-timestamps-ok--disable)))

(use-package org-roam-ok
  :custom ((org-roam-ok-node-use-cache-in-memory nil)
           (org-roam-ok-on-idle-delay nil)))

(use-package adaptive-wrap)

(use-package ok-plural)

(use-package org-roam-cjk
  :bind ("C-c n s" . org-roam-cjk-keyword-search-buffer)
  :custom ((org-roam-cjk-rg-section-file-truncate-col 25))
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
;;
;; The primal use of `org-ref' over `org-cite' is assumed.

(use-package org-roam-bibtex
  :after org-roam
  :custom ((orb-insert-link-description "${author-abbrev} ${date}")
           (orb-roam-ref-format 'org-ref-v3))
  :commands org-roam-bibtex-ok-link-auto-fill
  :config
  (require 'org-ref)        ; optional, if using Org-ref v2 or v3 citation links
  )

(use-package org-ref
  ;; For citations, cross-references, bibliographies.
  :custom (bibtex-completion-pdf-field "file"))

(use-package org-ref-ok
  :after org-ref)

(use-package org-ref-vis
  :straight nil
  :custom (org-ref-vis-style-getter
           (lambda (command lang)
             (let* ((chicago-en
                     (file-name-concat
                      org-ref-vis-csl-dir "styles"
                      ;; "chicago-fullnote-bibliography-short-title-subsequent.csl"
                      "chicago-note-bibliography.csl"))
                    ;; (chicago-ja
                    ;;  (file-name-concat
                    ;;   "~/github.com/okomestudio"
                    ;;   "chicago-fullnote-bibliography-short-title-subsequent-ja-csl"
                    ;;   "chicago-fullnote-bibliography-short-title-subsequent-ja.csl"))
                    (chicago-ja
                     (file-name-concat
                      "~/github.com/okomestudio/csl-chicago-ja"
                      "chicago-notes-bibliography-ja.csl")))
               (pcase lang
                 ("ja-JP" chicago-ja)
                 (_ chicago-en)))))
  :hook (org-mode . org-ref-vis-mode)
  :config
  (setopt org-ref-vis-command-alist
          (append
           '((("bibfullcite") . ((bib-entry) "${bib-entry}"))
             (("citepub") . ((title-only year-only)
                             "${title-only} (${publisher}, ${year-only})")))
           org-ref-vis-command-alist))

  (defun org-ref-vis-bibfullcite-export (path desc format _comm-channel)
    "Handle compilation export targets for the `bibfullcite' link.
PATH is the citekey string."
    (cond ((eq format 'latex)
           (format "\\bibfullcite{%s}"
                   (replace-regexp-in-string "^&" "" path)))
          ((eq format 'html)
           (format "<span class=\"citation\">[Full Cite: %s]</span>" path))
          (t (if desc desc (format "[Full Cite: %s]" path)))))

  (org-link-set-parameters
   "bibfullcite"
   :complete (lambda () (org-ref-cite-link-complete "bibfullcite"))
   :export #'org-ref-vis-bibfullcite-export
   :follow #'org-ref-click-hyperlink
   :help-echo #'org-ref-cite-tooltip)
  (add-to-list 'org-ref-cite-types '("bibfullcite" "Bibliography full list"))

  (defun org-ref-vis-citepub-export (path desc format _comm-channel)
    "Handle compilation export targets for the `citepub' link.
PATH is the citekey string."
    (cond ((eq format 'latex)
           (format "\\citepub{%s}"
                   (replace-regexp-in-string "^&" "" path)))
          ((eq format 'html)
           (format "<span class=\"citation\">[Cite Pub: %s]</span>" path))
          (t (if desc desc (format "[Cite Pub: %s]" path)))))

  (org-link-set-parameters
   "citepub"
   :complete (lambda () (org-ref-cite-link-complete "citepub"))
   :export #'org-ref-vis-citepub-export
   :follow #'org-ref-click-hyperlink
   :help-echo #'org-ref-cite-tooltip)
  (add-to-list 'org-ref-cite-types '("citepub" "Cite publication")))

(use-package bibtex-completion-ok)

;;; Misc.

(use-package org-id-ext)

(use-package or-struktur
  ;; Org Roam plugin for folgezettel IDs.
  :bind ( ([f9] . or-struktur-view-toggle)
          ("C-c s o" . or-struktur-view-show)
          :map or-struktur-view-mode-map
          ("i" . consult-org-heading) )
  ;; :custom (org-roam-ok-node-display-title #'or-struktur--display-title)
  :custom ((or-struktur-view-layout 'right)
           (or-struktur-view-show-title nil))
  :hook ((org-mode . or-struktur-mode)
         (or-struktur-view-mode
          . (lambda ()
              (interactive)
              (setq-local repeat-mode nil) ; this mode is sluggish
              (setq-local truncate-lines t)
              (setq-local org-use-speed-commands nil)
              (text-scale-set -0.4))))
  :config
  (when (bound-and-true-p desktop-save-mode)
    (add-to-list 'desktop-minor-mode-table '(or-struktur-mode nil)))

  (defun or-struktur--display-title (node)
    "Render NODE title with folgezettel for display."
    (concat
     (org-roam-ok-node--title node)
     (when-let* ((r (or-struktur-overlay--format (org-roam-node-id node))))
       (concat " " (propertize r 'face 'or-struktur-overlay))))))

(provide 'maj-org-roam)
;;; maj-org-roam.el ends here
