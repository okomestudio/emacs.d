;;; init-org.el --- Org  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ts/org-books-file "~/.books.org"
  "Default org-books-file."
  :type '(string)
  :group 'ts)

(defcustom ts/org-default-notes-file "~/.notes.org"
  "Default org-default-notes-file."
  :type '(string)
  :group 'ts)

(straight-use-package 'org)

(use-package org
  :after (ob-typescript ox-gfm)
  :ensure org-contrib

  :bind
  (("C-c l" . 'org-store-link)
   ("M-S q" . 'org-unfill-paragraph))

  :config
  (defun org-unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override 'fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (org-fill-paragraph nil region)))

  (defun org-ensure-all-headings-with-ids ()
    "Ensure all headings have IDs."
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (while (outline-previous-heading)
        (org-id-get-create))))

  (plist-put org-format-latex-options :scale 1.5)

  ;; Add characters allowed to bound emphasis markup.
  (setcar org-emphasis-regexp-components "-—[:space:]('\"{\x200B|│")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "-—[:space:].,:!?;'\")}\\[\x200B|│")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; For document export
  (require 'ox-md)                      ; Markdown
  (require 'ox-gfm)                     ; GitHub-flavored Markdown

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (dot . t)
     (emacs-lisp . t)
     (js . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     (sql . t)
     (typescript . t)))

  (custom-set-faces
   ;; Code block
   '(org-block-begin-line
     ((t (:foreground "#999999" :background "#f1ede5" :extend t))))
   '(org-block
     ((t (:background "#fbf6ed" :extend t))))
   '(org-block-end-line
     ((t (:foreground "#999999" :background "#f1ede5" :extend t))))
   '(org-modern-bracket-line
     ((t (:foreground "#999999" :background "#f1ede5" :extend t))))

   ;; Drawer
   '(org-drawer
     ((t (:foreground "#999999" :height 1.0 :inherit 'fixed-pitch))))
   '(org-special-keyword
     ((t (:foreground "#999999" :height 1.0 :inherit 'fixed-pitch))))
   '(org-property-value
     ((t (:foreground "#999999" :height 1.0 :inherit 'fixed-pitch))))

   ;; Table
   '(org-table
     ((t (:inherit 'variable-pitch))))

   ;; Code-like comments
   '(font-lock-comment-face
     ((t (:inherit 'fixed-pitch))))
   )

  :custom
  ((fill-column 80)
   (org-adapt-indentation nil)
   (org-babel-python-command "~/.pyenv/shims/python")
   (org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
   (org-capture-templates '(("b" "Book" entry (file ts/org-books-file)
                             "%(let* ((url (substring-no-properties (current-kill 0)))
                                      (details (org-books-get-details url)))
                                      (when details (apply #'org-books-format 1 details)))")
                            ("t" "Task" entry (file+headline "" "Tasks")
		                         "* TODO %?\n  %u\n  %a") ))
   (org-default-notes-file ts/org-default-notes-file)
   (org-ellipsis "⮷")
   (org-export-with-broken-links t)
   (org-export-with-section-numbers nil)
   (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
   (org-hide-emphasis-markers t)
   (org-image-actual-width nil)
   ;; (org-indent-indentation-per-level 4)
   (org-list-allow-alphabetical t)
   (org-list-indent-offset 2)
   ;; (org-plantuml-jar-path ts/path-plantuml)
   (org-preview-latex-image-directory ".ltximg/")
   (org-return-follows-link t)
   (org-startup-folded nil)
   (org-startup-indented t)
   (org-support-shift-select t)
   (org-tags-column 0)
   (org-todo-keywords '((sequence "TODO" "WIP" "|" "SKIP" "DONE"))))

  :hook
  ((org-mode . (lambda ()
                 (org-superstar-mode 1)
                 (turn-on-visual-line-mode)))))

(use-package org-agenda
  :after (org)
  :straight nil
  :custom
  (org-agenda-current-time-string "⭠ NOW ────────────────────")
  (org-agenda-include-diary t)
  (org-agenda-inhibit-startup t)
  (org-agenda-start-on-weekday 0)
  (org-agenda-use-tag-inheritance t)    ; set nil to speed up parsing

  :init
  (put 'org-agenda-custom-commands 'safe-local-variable #'listp))

(use-package org-books
  ;;Reading list management with org mode.
  :custom (org-books-file ts/org-books-file))

(use-package org-modern
  ;; Modern Org Style.
  :init (global-org-modern-mode)
  :custom
  (org-modern-block nil)
  (org-modern-checkbox '((?X . #("▢𐄂" 0 2 (composition ((2)))))
                         (?- . #("▢–" 0 2 (composition ((2)))))
                         (?\s . #("▢" 0 1 (composition ((1)))))))
  (org-modern-hide-stars 'nil)
  (org-modern-keyword "‣ ")
  (org-modern-priority t)
  (org-modern-star nil)
  (org-modern-statistics t)
  (org-modern-table nil)
  (org-modern-tag t)
  (org-modern-timestamp t)
  (org-modern-todo t)
  (org-modern-variable-pitch t))

(use-package org-modern-indent
  :straight
  (org-modern-indent :type git
                     :host github
                     :repo "jdtsmith/org-modern-indent")

  :config (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-ref
  ;; For citations, cross-references, bibliographies.
  :custom (bibtex-completion-pdf-field "file")
  :init (put 'bibtex-completion-bibliography 'safe-local-variable #'listp))

(use-package org-roam
  ;; Rudimentary Roam replica with org-mode.
  :after org
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)

  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)

  :custom
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?\n<%<%Y-%m-%d %a %H:%M>>"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-dailies-directory "journal/")
  (org-roam-db-location (file-truename "~/.config/emacs/roam/.roam.db"))
  (org-roam-directory (file-truename "~/.config/emacs/roam"))
  (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
  (org-roam-mode-sections (list #'org-roam-backlinks-section
                                #'org-roam-reflinks-section
                                #'org-roam-unlinked-references-section))
  (org-roam-node-annotation-function
   (lambda (node) (concat " " (marginalia--time (org-roam-node-file-mtime node)))))
  (org-roam-node-display-template
   (concat "${my-node-entry} ${my-node-parent-title} " (propertize "${tags}" 'face 'org-tag)))

  :init
  ;; Override some functions here:
  (eval-after-load 'org-roam-node
    '(cl-defmethod org-roam-node-my-node-entry ((node org-roam-node))
       (org-roam-node-title node)))

  (eval-after-load 'org-roam-node
    '(cl-defmethod org-roam-node-my-node-parent-title ((node org-roam-node))
       (if (string= (org-roam-node-title node) (org-roam-node-file-title node))
           ""
         (concat
          (propertize "< "
                      'face '(:foreground "dim gray"))
          (propertize (org-roam-node-file-title node)
                      'face '(:slant italic :foreground "dim gray" :height 1.0 :underline t)))
         )
       ))

  (eval-after-load 'org-roam-node
    '(cl-defmethod org-roam-node-slug ((node org-roam-node))
       "Return the slug of NODE. Overridden to use hyphens instead of underscores."
       (let ((title (org-roam-node-title node))
             (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                                768   ; U+0300 COMBINING GRAVE ACCENT
                                769   ; U+0301 COMBINING ACUTE ACCENT
                                770   ; U+0302 COMBINING CIRCUMFLEX ACCENT
                                771   ; U+0303 COMBINING TILDE
                                772   ; U+0304 COMBINING MACRON
                                774   ; U+0306 COMBINING BREVE
                                775   ; U+0307 COMBINING DOT ABOVE
                                776   ; U+0308 COMBINING DIAERESIS
                                777   ; U+0309 COMBINING HOOK ABOVE
                                778   ; U+030A COMBINING RING ABOVE
                                779   ; U+030B COMBINING DOUBLE ACUTE ACCENT
                                780   ; U+030C COMBINING CARON
                                795   ; U+031B COMBINING HORN
                                803   ; U+0323 COMBINING DOT BELOW
                                804   ; U+0324 COMBINING DIAERESIS BELOW
                                805   ; U+0325 COMBINING RING BELOW
                                807   ; U+0327 COMBINING CEDILLA
                                813   ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                                814   ; U+032E COMBINING BREVE BELOW
                                816   ; U+0330 COMBINING TILDE BELOW
                                817   ; U+0331 COMBINING MACRON BELOW
                                )))
         (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                    (strip-nonspacing-marks (s) (string-glyph-compose
                                                 (apply #'string
                                                        (seq-remove #'nonspacing-mark-p
                                                                    (string-glyph-decompose s)))))
                    (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
           (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                           ("--*" . "-") ;; remove sequential underscores
                           ("^-" . "")   ;; remove starting underscore
                           ("-$" . ""))) ;; remove ending underscore
                  (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
             (downcase slug))))) )

  (put 'orb-preformat-keywords 'safe-local-variable #'listp)
  (put 'org-roam-capture-templates 'safe-local-variable #'listp)
  (put 'org-roam-dailies-capture-templates 'safe-local-variable #'listp)
  (put 'org-roam-db-location 'safe-local-variable #'stringp)
  (put 'org-roam-directory 'safe-local-variable #'stringp)
  (put 'org-roam-mode-sections 'safe-local-variable #'listp)
  (put 'org-roam-ui-port 'safe-local-variable #'integerp))

(use-package org-roam-bibtex
  :after org-roam
  :config (require 'org-ref) ; optional: if using Org-ref v2 or v3 citation links
  :custom
  (orb-insert-link-description "${author-abbrev} ${date}")
  (orb-roam-ref-format 'org-ref-v3))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-follow t)
  (org-roam-ui-sync-theme t)
  (org-roam-ui-update-on-save t))

(use-package org-sticky-header
  :init (org-sticky-header-mode +1))

(use-package org-superstar
  :custom (org-superstar-headline-bullets-list '("◉" "🞛" "○" "▷")))

(use-package ob-typescript)
(use-package ox-gfm)


(defcustom ts/max-buffer-size 100000
  "Default max-buffer-size over which valign-mode will not activate."
  :type '(integer)
  :group 'ts)

(use-package valign
  ;; Pixel-perfect visual alignment for Org and Markdown tables.

  :custom
  (valign-fancy-bar t)
  (valign-max-table-size 4000)
  (valign-signal-parse-error t)

  :init
  ;; Add logic to avoid loading valign-mode for large buffers.
  (add-hook 'org-mode-hook
            (lambda ()
              (when (not (> (buffer-size) ts/max-buffer-size))
                (valign-mode)
                )
              )
            )

  :config
  (use-package ftable)
  )

(provide 'init-org)
;;; init-org.el ends here
