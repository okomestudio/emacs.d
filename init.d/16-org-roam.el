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
             ;; :repo "org-roam/org-roam"

             ;; Use the fork for development:
             :repo "okomestudio/org-roam"
             :fork "okomestudio"
             :branch "refactor-org-roam-unlinked-references-section"

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
                                #'org-roam-unlinked-references-section
                                ;; #'org-roam-plugin-ja-unlinked-references-section
                                ))
  (org-roam-node-display-template (concat "​​​​​${my-node-entry:*}"
                                          ;; (propertize "${my-node-tags:16}" 'face 'org-tag)
                                          (propertize "${tags:16}" 'face 'org-tag)
                                          " ${my-node-timestamp:*}"))

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
  (org-roam-db-autosync-mode)

  ;; TITLE LISTING IN MINIBUFFER
  (with-eval-after-load 'org-roam-node
    (defvar ok-org-roam--file-node-cache '()
      "Cache file nodes.")

    (defun ok-org-roam--get-node-id-from-file (file)
      (caar (org-roam-db-query `[:select nodes:id :from nodes
                                         :where (and (= nodes:file ,file)
                                                     (= nodes:level 0))])))

    (defun ok-org-roam-file-node-cache-maybe-invalidate ()
      (let ((file buffer-file-name))
        (when (string= (file-name-extension file) "org")
          (setf ok-org-roam--file-node-cache (assoc-delete-all file ok-org-roam--file-node-cache)))))

    (add-hook 'after-save-hook #'ok-org-roam-file-node-cache-maybe-invalidate)

    (defun ok-org-roam--get-node-from-file (file)
      (let ((cached (assoc file ok-org-roam--file-node-cache)))
        (if cached
            (cdr (cdr cached))
          (let ((node (org-roam-node-from-id (ok-org-roam--get-node-id-from-file file))))
            (push `(,file . (,(float-time) . ,node)) ok-org-roam--file-node-cache)
            node))))

    (defun ok-org-roam-visit-parent-at-point (node)
      "Visit parent of given NODE at point, if exists."
      (interactive "P")
      (let ((parent (cdr (assoc-string "PARENT"
                                       (org-roam-node-properties
                                        (if node
                                            node
                                          (org-roam-node-at-point)))))))
        (if parent
            (org-link-open-from-string parent)
          (message "No parent found"))))

    (defun ok-org-roam--get-parent-title (node)
      (let ((parent (cdr (assoc-string "PARENT" (org-roam-node-properties node)))))
        (when parent
          ;; NOTE: This replacement may not be necessary, but some links are not
          ;; rendered correctly in minibuffer without. For now, the slow down due
          ;; to parsing is not significant.
          (replace-regexp-in-string "\\[\\[\\(.+\\)\\]\\[\\([^]]+\\)\\]\\]"
                                    "\\2"
                                    parent))))

    (defun ok-org-roam--get-title-aux (node)
      (let* ((node-title (org-roam-node-title node))
             (node-file-title (or (if (not (s-blank? (org-roam-node-file-title node)))
                                      (org-roam-node-file-title node))
                                  (file-name-nondirectory (org-roam-node-file node)))))
        (if (string= node-title node-file-title)
            (let ((p (ok-org-roam--get-parent-title node)))
              (if p (list " ❬ " p)))
          (if (member node-title (org-roam-node-aliases node))
              (list " = " node-file-title)
            (let ((p (ok-org-roam--get-parent-title (ok-org-roam--get-node-from-file (org-roam-node-file node)))))
              (if p (list " ❬ " p) (list " ❬ " node-file-title)))))))

    (defun ok-org-roam--render-title-aux (title-aux)
      (if (not title-aux)
          ""
        (let ((sym (nth 0 title-aux))
              (aux (nth 1 title-aux))
              (face-sym `(;; symbol
                          :foreground ,(face-attribute 'completions-annotations
                                                       :foreground)))
              (face-aux `(;; text
                          :foreground ,(face-attribute 'completions-annotations
                                                       :foreground)
                          :slant italic)))
          (concat (propertize sym 'face face-sym)
                  (propertize aux 'face face-aux)))))

    (cl-defmethod org-roam-node-my-node-entry ((node org-roam-node))
      (concat (org-roam-node-title node) (ok-org-roam--render-title-aux (ok-org-roam--get-title-aux node))))

    (cl-defmethod org-roam-node-my-node-tags ((node org-roam-node))
      (let ((tags (org-roam-node-tags node)))
        (when tags
          (format ":%s:" (string-join tags ":")))))

    (cl-defmethod org-roam-node-my-node-timestamp ((node org-roam-node))
      (require 'marginalia)
      (marginalia--time
       (let ((node-mtime (cdr (assoc "MTIME" (org-roam-node-properties node))))
             (inhibit-message t))
         (if node-mtime
             (progn
               (require 'org-roam-timestamps)
               (org-roam-timestamps-encode (car (split-string node-mtime))))
           (org-roam-node-file-mtime node)))))

    (cl-defmethod org-roam-node-slug ((node org-roam-node))
      "Return the slug of NODE. Overridden to use hyphens instead of underscores."
      (orp-string-to-org-slug (org-roam-node-title node)))))


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


(use-package adaptive-wrap
  :hook
  (org-roam-mode . (lambda ()
                     (turn-on-visual-line-mode)
                     ;; Format org-roam buffer so that unlinked reference list
                     ;; are easier to see.
                     (setq-local adaptive-wrap-extra-indent 4)
                     (adaptive-wrap-prefix-mode +1))))

;; Local Variables:
;; nameless-aliases: (("" . "ok-org-roam"))
;; End:
;;; 16-org-roam.el ends here
