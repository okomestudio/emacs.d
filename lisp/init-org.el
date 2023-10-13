;;; init-org.el --- Org  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package org
  :after (ob-typescript)
  :ensure org-contrib

  :bind
  (("C-c l" . 'org-store-link)
   ("M-S q" . 'org-unfill-paragraph))

  :custom
  (fill-column 80)
  (org-adapt-indentation nil)
  (org-babel-python-command "~/.pyenv/shims/python")
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (org-ellipsis "⮷")
  (org-export-with-broken-links t)
  (org-export-with-section-numbers nil)
  (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  (org-list-allow-alphabetical t)
  (org-list-indent-offset 2)
  ;; (org-plantuml-jar-path ts/path-plantuml)
  (org-preview-latex-image-directory ".ltximg/")
  (org-return-follows-link t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-support-shift-select t)
  (org-tags-column 0)
  (org-todo-keywords '((sequence "TODO" "WIP" "|" "SKIP" "DONE")))

  :ensure-system-package
  (latex . "sudo apt install -y texlive texlive-latex-extra texlive-lang-cjk texlive-extra-utils texlive-luatex")
  (pdfcropmargins . "pip install pdfCropMargins")

  :hook
  (org-mode . (lambda () (org-superstar-mode 1) (turn-on-visual-line-mode)))

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

  (defun org-interpolate-leaf-nodes-for-export ()
    "Extrapolate leaf heading nodes for export.

When invoked within an Org buffer, the headings are traversed in
its copy, each leaf heading expanded with the body of the target
node."
    (interactive)
    (let* ((tmp-buffer (org-export-copy-buffer)))
      (with-current-buffer tmp-buffer
        (beginning-of-buffer)
        (while (outline-next-heading)
          (while (org-goto-first-child) t)
          (end-of-line)
          (backward-char)
          (when (link-hint--org-link-at-point-p)
            (let* ((has-content nil))
              (save-excursion
                (org-open-at-point +1)
                (beginning-of-line)
                (if (eq ?* (char-after))
                    (setq has-content t))
                (with-current-buffer (current-buffer)
                  (org-preserve-local-variables
                   (let* ((end (org-end-of-subtree t t)))
                     (previous-line)
                     (org-back-to-heading)
                     (copy-region-as-kill (re-search-forward "^\\s-*$") end)))
                  (kill-buffer)))
              (end-of-line)
              (org-return-and-maybe-indent)
              (when has-content
                (org-yank))))))
      (switch-to-buffer tmp-buffer)))

  ;; Update regex for org emphasis; see, e.g.,
  ;; https://stackoverflow.com/a/63805680/515392.
  (setcar org-emphasis-regexp-components
          (concat (string ?- ?—)
                  "[:space:][:nonascii:]"
                  (string ?\N{ZERO WIDTH SPACE}
                          ?\[ ?\( ?{
                          ?‘ ?“
                          ?| ?│ )
                  ""))
  (setcar (nthcdr 1 org-emphasis-regexp-components)
          (concat (string ?\] ?\) ?})
                  "[:space:][:nonascii:]"
                  (string ?\N{ZERO WIDTH SPACE}
                          ?’ ?”
                          ?| ?│
                          ?. ?, ?? ?! ?\; ?:
                          ?— ?- )
                  ""))
  (setcar (nthcdr 2 org-emphasis-regexp-components)
          (concat "[:space:]"))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Babel
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (dot . t)
                               (emacs-lisp . t)
                               (js . t)
                               (plantuml . t)
                               (python . t)
                               (shell . t)
                               (sql . t)
                               (sqlite . t)
                               (typescript . t)))
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

  ;; Color styling
  (custom-set-faces
   ;; Code block
   '(org-block-begin-line ((t (:foreground "#999999" :background "#f1ede5" :extend t))))
   '(org-block ((t (:background "#fbf6ed" :extend t))))
   '(org-block-end-line ((t (:foreground "#999999" :background "#f1ede5" :extend t))))
   '(org-modern-bracket-line ((t (:foreground "#999999" :background "#f1ede5" :extend t))))
   ;; Drawer
   '(org-drawer ((t (:foreground "#999999" :height 1.0 :inherit 'fixed-pitch))))
   '(org-special-keyword ((t (:foreground "#999999" :height 1.0 :inherit 'fixed-pitch))))
   '(org-property-value ((t (:foreground "#999999" :height 1.0 :inherit 'fixed-pitch))))
   ;; Table
   '(org-table ((t (:inherit 'fixed-pitch))))

   ;; Code-like comments
   '(font-lock-comment-face ((t (:inherit 'fixed-pitch)))))

  ;; (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  ;; (add-to-list 'org-latex-packages-alist '("" "unicode-math"))
  (setq org-preview-latex-default-process 'lualatexpdf)
  (setq org-preview-latex-process-alist
        '((lualatexpdf :programs ("lualatex" "dvisvgm")
                    :description "pdf > svg"
                    :message "you need to install the programs: lualatex and dvisvgm."
                    :image-input-type "pdf"
                    :image-output-type "svg"
                    :image-size-adjust (1.7 . 1.5)
                    :latex-compiler ("lualatex -interaction nonstopmode --shell-escape -output-directory %o %f")
                    :image-converter ("pdfcropmargins -v -p 0 -a -5 %f -o /tmp/cropped.pdf ; dvisvgm -P /tmp/cropped.pdf -n -b min -c %S -o %O"))
          (lualatexdvi :programs ("lualatex" "dvisvgm")
                    :description "dvi > svg"
                    :message "you need to install the programs: lualatex and dvisvgm."
                    :image-input-type "dvi"
                    :image-output-type "svg"
                    :image-size-adjust (1.7 . 1.5)
                    :latex-compiler ("dvilualatex -interaction nonstopmode --shell-escape -output-directory %o %f")
                    :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))
  )


(use-package ox
  ;; Org export.
  :after (org ox-gfm)
  :straight nil

  :config
  (require 'ox-md)  ;; Markdown
  (require 'ox-gfm) ;; GitHub-flavored Markdown

  (defun init-org--org-html-link (link desc info)
    (let* ((raw-link (org-element-property :raw-link link))
           (raw-path (org-element-property :path link))
           (type (org-element-property :type link))
           (link-is-url (member type '("http" "https" "ftp" "mailto")))
           (desc (org-string-nw-p desc)))
      (if link-is-url
          (format "<a href=\"%s\">%s</a>" raw-link (if desc desc raw-link))
        (if (string= (substring raw-link 0 3) "id:")
            desc
          (if (member (file-name-extension raw-link) '("gif" "jpeg" "jpg" "png" "webp"))
              (format "<img src=\"%s\" />" raw-link)
            (format "<a href=\"%s\">%s</a>" raw-link desc))))))

  (org-export-define-derived-backend 'substack 'html
    :menu-entry
    '(?S "Export to Substack article"
         ((?o "As HTML file and open"
	            (lambda (a s v b)
	              (if a
                    (org-export-to-buffer t s v b)
                  (let* ((f (concat (file-name-sans-extension buffer-file-name) ".html")))
                    (org-open-file (org-export-to-file 'substack f nil s v b))))))))

    :translate-alist
    '((link . init-org--org-html-link))))


(use-package ox-gfm :after ox)
(use-package ox-hugo :after ox)


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
  ;; Reading list management with org mode.
  :disabled

  :init
  (defcustom ts/org-books-file "~/.config/emacs/.books.org"
    "Default org-books-file."
    :type '(string)
    :group 'ts)

  :custom
  (org-books-file ts/org-books-file))


(use-package org-modern
  ;; Modern Org Style.
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
  (org-modern-variable-pitch t)

  :init
  (with-eval-after-load 'org
    (global-org-modern-mode)))


(use-package org-modern-indent
  :straight
  (org-modern-indent :type git
                     :host github
                     :repo "jdtsmith/org-modern-indent")

  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))


(use-package org-side-tree)


(use-package org-sticky-header
  :init
  (org-sticky-header-mode +1))


(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '("◉" "🞛" "○" "▷")))


(use-package org-transclusion
  :bind
  (:map org-mode-map
   ("C-c C-n a" . org-transclusion-add)
   ("C-c C-n t" . org-transclusion-mode)))


(use-package ob-typescript)


;; Org table styling

(use-package valign
  ;; Pixel-perfect visual alignment for Org and Markdown tables.
  :custom
  (valign-fancy-bar t)
  (valign-max-table-size 4000)
  (valign-signal-parse-error t)

  :preface
  (defcustom valign-max-buffer-size 100000
    "Default max-buffer-size over which valign-mode will not activate."
    :type '(integer)
    :group 'ts)

  :init
  ;; Add logic to avoid loading valign-mode for large buffers.
  (add-hook 'org-mode-hook
            (lambda ()
              (when (not (> (buffer-size) valign-max-buffer-size))
                (custom-set-faces '(org-table ((t (:inherit 'variable-pitch)))))
                (valign-mode))))

  :config
  (use-package ftable))


;;; Org Roam

(use-package org-roam
  :after (org adaptive-wrap)

  :straight
  (:type git :host github :repo "okomestudio/org-roam"
   :branch "okomestudio"
   :fork "okomestudio")

  :bind
  (("C-c n c" . (lambda () (interactive) (org-capture nil "f")))
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)

   :map org-mode-map
   ("C-M-i" . completion-at-point))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)

  :custom
  (org-roam-completion-everywhere nil)
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %?\n<%<%Y-%m-%d %a %H:%M>>"
                                         :target
                                         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-dailies-directory "journal/")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-location (file-truename "~/.config/emacs/roam/.roam.db"))
  (org-roam-directory (file-truename "~/.config/emacs/roam"))
  (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
  (org-roam-mode-sections (list #'org-roam-backlinks-section
                                #'org-roam-reflinks-section
                                #'org-roam-unlinked-references-section))
  (org-roam-node-display-template (concat "​​​​​${my-node-entry:*}"
                                          (propertize "${tags:16}" 'face 'org-tag)
                                          " ${my-node-timestamp:*}"))
  (org-roam-unlinked-references-word-boundary-re
   "|(\\b%1$s\\b|(?<=[^\x20-\x7e\xff61-\xff9f])%1$s(?=[^\x20-\x7e\xff61-\xff9f]))")

  :hook
  (org-roam-mode . (lambda ()
                     (visual-line-mode +1)
                     (setq-local adaptive-wrap-extra-indent 4)
                     (adaptive-wrap-prefix-mode +1)))

  :preface
  (put 'orb-preformat-keywords 'safe-local-variable #'listp)
  (put 'org-roam-capture-templates 'safe-local-variable #'listp)
  (put 'org-roam-dailies-capture-templates 'safe-local-variable #'listp)
  (put 'org-roam-db-location 'safe-local-variable #'stringp)
  (put 'org-roam-directory 'safe-local-variable #'stringp)
  (put 'org-roam-mode-sections 'safe-local-variable #'listp)
  (put 'org-roam-ui-port 'safe-local-variable #'integerp)

  :init
  (with-eval-after-load 'org-roam-node
    (cl-defmethod org-roam-node-my-node-timestamp ((node org-roam-node))
      (marginalia--time
       (let* ((node-properties (org-roam-node-properties node))
              (node-mtime (cdr (assoc "MTIME" node-properties)))
              (inhibit-message t))
         (if node-mtime
             (org-roam-timestamps-encode (car (split-string node-mtime)))
           (org-roam-node-file-mtime node)))))


    (defun init-org--get-node-id-from-file (file)
      (caar (org-roam-db-query `[:select nodes:id :from nodes :where (and (= nodes:file ,file) (= nodes:level 0))])))

    (setq init-org--file-node-cache '())

    (defun init-org--get-node-from-file (file)
      (let ((x (assoc file init-org--file-node-cache)))
        (if x
            (let* ((ttl 30.0)
                   (v (cdr x))
                   (tt (car v))
                   (dt (- (float-time) tt)))
              (if (< dt ttl)
                  (car (cdr v))
                (setf init-org--file-node-cache (assoc-delete-all file init-org--file-node-cache))
                (init-org--get-node-from-file file)))
          (let ((y (org-roam-node-from-id (init-org--get-node-id-from-file file))))
            (add-to-list 'init-org--file-node-cache `(,file ,(float-time) ,y))
            y))))

    (defun init-org--get-parent-title (node)
      (let ((parent (cdr (assoc-string "PARENT" (org-roam-node-properties node)))))
        (when parent
          (replace-regexp-in-string "\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\]"
                                    "\\2"
                                    parent))))

    (cl-defmethod org-roam-node-my-node-entry ((node org-roam-node))
      (let* ((title-annotate-color "SeaGreen4")
             (node-title (org-roam-node-title node))
             (node-file-title (or (if (not (s-blank? (org-roam-node-file-title node)))
                                      (org-roam-node-file-title node))
                                  (file-name-nondirectory (org-roam-node-file node))))
             (title-aux (if (string= node-title node-file-title)
                            (let ((x (init-org--get-parent-title node)))
                              (if x (list " ❬ " x)))
                          (if (member node-title (org-roam-node-aliases node))
                              (list " = " node-file-title)
                            (let ((x (init-org--get-parent-title (init-org--get-node-from-file (org-roam-node-file node)))))
                              (if x (list " ❬ " x) (list " ❬ " node-file-title)))
                            ))))
        (concat
         node-title
         (if (not title-aux)
             ""
           (let ((sym (nth 0 title-aux))
                 (aux (nth 1 title-aux)))
             (concat
              (propertize sym 'face `(:foreground ,title-annotate-color))
              (propertize aux 'face `(:foreground ,title-annotate-color :slant italic))))))))

    (defun ts/org-roam-node-slug (title)
      (let* (;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
            (slug-trim-chars '(768    ; U+0300 COMBINING GRAVE ACCENT
                               769    ; U+0301 COMBINING ACUTE ACCENT
                               770    ; U+0302 COMBINING CIRCUMFLEX ACCENT
                               771    ; U+0303 COMBINING TILDE
                               772    ; U+0304 COMBINING MACRON
                               774    ; U+0306 COMBINING BREVE
                               775    ; U+0307 COMBINING DOT ABOVE
                               776    ; U+0308 COMBINING DIAERESIS
                               777    ; U+0309 COMBINING HOOK ABOVE
                               778    ; U+030A COMBINING RING ABOVE
                               779    ; U+030B COMBINING DOUBLE ACUTE ACCENT
                               780    ; U+030C COMBINING CARON
                               795    ; U+031B COMBINING HORN
                               803    ; U+0323 COMBINING DOT BELOW
                               804    ; U+0324 COMBINING DIAERESIS BELOW
                               805    ; U+0325 COMBINING RING BELOW
                               807    ; U+0327 COMBINING CEDILLA
                               813    ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                               814    ; U+032E COMBINING BREVE BELOW
                               816    ; U+0330 COMBINING TILDE BELOW
                               817))) ; U+0331 COMBINING MACRON BELOW
        (cl-flet* ((nonspacing-mark-p (char)
                     (memq char slug-trim-chars))
                   (strip-nonspacing-marks (s)
                     (string-glyph-compose
                      (apply #'string
                             (seq-remove #'nonspacing-mark-p
                                         (string-glyph-decompose s)))))
                   (cl-replace (title pair)
                     (replace-regexp-in-string (car pair) (cdr pair) title)))
          (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                          ("--*" . "-") ;; remove sequential underscores
                          ("^-" . "")   ;; remove starting underscore
                          ("-$" . ""))) ;; remove ending underscore
                 (slug (-reduce-from #'cl-replace
                                     (strip-nonspacing-marks title)
                                     pairs)))
            (downcase slug)))))

    (cl-defmethod org-roam-node-slug ((node org-roam-node))
      "Return the slug of NODE. Overridden to use hyphens instead of underscores."
      (ts/org-roam-node-slug (org-roam-node-title node))))

  :config
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
  :custom
  (bibtex-completion-pdf-field "file")

  :preface
  (put 'bibtex-completion-bibliography 'safe-local-variable #'listp))


(use-package org-roam-timestamps
  :after org-roam

  :custom
  (org-roam-timestamps-minimum-gap 43200)
  (org-roam-timestamps-parent-file nil)
  (org-roam-timestamps-remember-timestamps t)

  :config
  (org-roam-timestamps-mode))


(use-package adaptive-wrap
  :custom
  (adaptive-wrap-extra-indent 4))


(provide 'init-org)
;;; init-org.el ends here
