;;; 15-org.el --- org  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Org and related utilities.
;;
;;; Code:

(use-package org
  :ensure org-contrib
  :bind
  (;; no globals
   :map org-mode-map
   ("C-c C-l" . 'org-insert-link)
   ("C-c l" . 'org-store-link)
   ("M-g i" . 'consult-org-heading)
   ("M-q" . 'okutil-org-fill-or-unfill-paragraph))

  :custom
  (org-adapt-indentation nil)
  (org-babel-python-command "~/.pyenv/shims/python")
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-ellipsis "⮷")
  (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  (org-imenu-depth 6)
  (org-list-allow-alphabetical t)
  (org-list-indent-offset 2)
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
  (org-mode . (lambda ()
                (setq-local fill-column 80)
                (turn-on-visual-line-mode)))

  :config
  ;; HELPER FUNCTIONS
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

  ;; EMPHASIS
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

  ;; BABEL
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (use-package ob-typescript)
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

  ;; LATEX PREVIEW
  ;; (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  ;; (add-to-list 'org-latex-packages-alist '("" "unicode-math"))
  (setq org-preview-latex-default-process 'lualatexpdf)
  (add-to-list 'org-preview-latex-process-alist
               '(lualatexpdf
                 :programs ("lualatex" "dvisvgm")
                 :description "pdf > svg"
                 :message "you need to install the programs: lualatex and dvisvgm."
                 :image-input-type "pdf"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("lualatex -interaction nonstopmode --shell-escape -output-directory %o %f")
                 :image-converter ("pdfcropmargins -v -p 0 -a -5 %f -o /tmp/cropped.pdf ; dvisvgm -P /tmp/cropped.pdf -n -b min -c %S -o %O")))
  (add-to-list 'org-preview-latex-process-alist
               '(lualatexdvi
                 :programs ("lualatex" "dvisvgm")
                 :description "dvi > svg"
                 :message "you need to install the programs: lualatex and dvisvgm."
                 :image-input-type "dvi"
                 :image-output-type "svg"
                 :image-size-adjust (1.7 . 1.5)
                 :latex-compiler ("dvilualatex -interaction nonstopmode --shell-escape -output-directory %o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))))


(use-package ox
  ;; Org export.
  :after (org)
  :straight nil
  :custom
  (org-export-with-broken-links t)
  (org-export-with-section-numbers nil)

  :config
  (require 'ox-md)  ;; Markdown
  (require 'ox-gfm) ;; GitHub-flavored Markdown

  (defun ok-org--org-html-link (link desc info)
    (let* ((raw-link (org-element-property :raw-link link))
           (raw-path (org-element-property :path link))
           (type (org-element-property :type link))
           (link-is-url (member type '("http" "https" "ftp" "mailto")))
           (desc (org-string-nw-p desc)))
      (if link-is-url
          (format "<a href=\"%s\">%s</a>" raw-link (or desc raw-link))
        (if (string= (substring raw-link 0 3) "id:")
            desc
          (if (member (file-name-extension raw-link)
                      '("gif" "jpeg" "jpg" "png" "webp"))
              (format "<img src=\"%s\" />" raw-link)
            (format "<a href=\"%s\">%s</a>" raw-link desc))))))

  (org-export-define-derived-backend
   'substack 'html
   :menu-entry
   '(?S "Export to Substack article"
        ((?o "As HTML file and open"
	           (lambda (a s v b)
	             (if a
                   (org-export-to-buffer t s v b)
                 (let ((f (concat (file-name-sans-extension buffer-file-name)
                                  ".html")))
                   (org-open-file (org-export-to-file 'substack f nil s v b))))))))

   :translate-alist
   '((link . ok-org--org-html-link))))


(use-package ox-gfm :after ox)
(use-package ox-hugo :after ox)


(use-package org-agenda
  :straight nil
  :after (org)
  :custom
  (org-agenda-current-time-string "⭠ NOW ────────────────────")
  (org-agenda-include-diary t)
  (org-agenda-inhibit-startup t)
  (org-agenda-start-on-weekday 0)
  (org-agenda-use-tag-inheritance t)    ; set nil to speed up parsing

  :preface
  (put 'org-agenda-custom-commands 'safe-local-variable #'listp))


(use-package org-modern
  ;; Modern Org Style.
  :after (org)
  :custom
  (org-modern-block-name t) ;; use org-modern-indent
  (org-modern-checkbox '((?X . #("▢𐄂" 0 2 (composition ((2)))))
                         (?- . #("▢–" 0 2 (composition ((2)))))
                         (?\s . #("▢" 0 1 (composition ((1)))))))
  (org-modern-hide-stars 'nil)
  (org-modern-keyword "‣ ")
  (org-modern-list '((?+ . "▷")
                     (?- . "𑁋") ;; "–"
                     (?* . "▶")))
  (org-modern-priority t)
  (org-modern-star '("◉" "🞛" "○" "▷"))
  (org-modern-statistics t)
  (org-modern-table nil)
  (org-modern-tag t)
  (org-modern-timestamp t)
  (org-modern-todo t)

  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))


(use-package org-modern-indent
  ;; Modern block styling with org-indent.
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :init (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  :hook
  (org-mode . (lambda()
                ;; See github.com/jdtsmith/org-modern-indent/issues/10
                (require 'org-indent)
                (if org-indent--text-line-prefixes
                    (aset org-indent--text-line-prefixes
                          0 (propertize " " 'face 'org-indent))))))


(use-package org-side-tree
  :disabled)


(use-package org-transclusion
  :straight
  (;; Info manual isn't generated by default, so do it here:
   :pre-build
   (("make" "org-transclusion.org")
    ("make" "-C" "./docs" "org-transclusion.texi")
    ("makeinfo" "./docs/org-transclusion.texi" "-o" "./docs/org-transclusion.info")
    ("install-info" "./docs/org-transclusion.info" "./docs/dir")))

  :bind
  (;
   :map org-mode-map
   :prefix "C-c C-n"
   :prefix-map ok-org-transclusion-map
   ("A" . org-transclusion-add-all)
   ("D" . org-transclusion-remove-all)
   ("a" . org-transclusion-add)
   ("t" . org-transclusion-mode))

  :init
  (add-to-list 'Info-directory-list
               (expand-file-name "straight/build/org-transclusion/docs/"
                                 user-emacs-directory))

  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode))


(use-package org-web-tools
  :disabled)


(use-package valign
  ;; Pixel-perfect visual alignment for Org and Markdown tables.
  :custom
  (valign-fancy-bar t)
  (valign-max-table-size 4000)
  (valign-signal-parse-error t)

  :hook
  (org-mode . (lambda ()
                (when (<= (buffer-size) ok-org--valign-max-buffer-size)
                  (valign-mode 1))))

  :init
  (defvar ok-org--valign-max-buffer-size 100000
    "Default max-buffer-size above which valign-mode will not activate."))


;; FONT FACE

(defvar ok-org-fixed-pitch-faces '(font-lock-builtin-face
                             font-lock-comment-delimiter-face
                             font-lock-comment-face
                             font-lock-constant-face
                             font-lock-doc-face
                             font-lock-function-name-face
                             font-lock-keyword-face
                             font-lock-negation-char-face
                             font-lock-preprocessor-face
                             font-lock-regexp-grouping-backslash
                             font-lock-regexp-grouping-construct
                             font-lock-string-face
                             font-lock-type-face
                             font-lock-variable-name-face
                             org-block
                             org-block-begin-line
                             org-block-end-line
                             org-checkbox
                             org-code
                             org-document-info-keyword
                             org-drawer
                             org-formula
                             org-indent
                             org-latex-and-related
                             org-meta-line
                             org-modern-bracket-line
                             org-modern-tag
                             org-property-value
                             org-special-keyword
                             ;; org-table
                             org-verbatim)
  "Fixed-pitch faces in Org mode.")

(defvar ok-org--outline-faces '((org-level-1 . '(:height 1.24))
                          (org-level-2 . '(:height 1.12))
                          (org-level-3 . '(:height 1.00))
                          (org-level-4 . '(:height 0.90))
                          (org-level-5 . '(:height 0.90))
                          (org-level-6 . '(:height 0.90))
                          (org-level-7 . '(:height 0.90))
                          (org-level-8 . '(:height 0.90))
                          (org-document-title . '(:height 1.24)))
  "Base outlines faces used in Org mode.")

(with-eval-after-load 'org-modern-indent
  (defun ok-org--get-text-scale-mode-height ()
    (when (bound-and-true-p text-scale-mode)
      (car (cdr (assoc :height (cdr (assoc 'default face-remapping-alist)))))))

  (defun ok-org--remap-to-mixed-pitch ()
    (face-remap-add-relative 'default :inherit 'variable-pitch)

    (dolist (face ok-org-fixed-pitch-faces)
      (face-remap-add-relative face :inherit 'fixed-pitch)))

  (defun ok-org--handle-text-scale-mode ()
    (let ((height (ok-org--get-text-scale-mode-height)))
      (when height
        (dolist (face (append ok-org-fixed-pitch-faces
                              ok-org--outline-faces))
          (if (consp face)
              (setq face (car face)))
          (face-remap-add-relative face :height height)))))

  (add-hook 'org-mode-hook (lambda ()
                             (ok-org--remap-to-mixed-pitch)
                             (ok-org--handle-text-scale-mode)) 90)

  (dolist (it ok-org--outline-faces)
    (let* ((face (car it))
           (parent-face (face-attribute face :inherit))
           (prop (cdr it))
           (height (car (cdr (assoc :height prop))))
           (weight 'bold))
      (set-face-attribute parent-face nil :inherit 'ok-face-outline)
      (set-face-attribute face nil :height height :weight weight)))

  (set-face-attribute 'org-drawer nil :foreground (face-attribute 'shadow :foreground)))

;; Local Variables:
;; nameless-aliases: (("" . "ok-org"))
;; End:
;;; 15-org.el ends here
