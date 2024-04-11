;;; 15-org.el --- org  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Org and related utilities.
;;
;;; Code:

(use-package org
  :bind (nil
         :map org-mode-map
         ("C-c C-l" . org-insert-link)
         ("C-c C-q" . org-set-tags-command)
         ("C-c l" . org-store-link)
         ("M-g i" . consult-org-heading))
  :hook (org-mode . ok-org-mode-hook)
  :custom
  (org-adapt-indentation nil)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-ellipsis "⮷")
  (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  (org-imenu-depth 6)
  (org-list-allow-alphabetical t)
  (org-list-indent-offset 2)
  (org-M-RET-may-split-line '((headline . nil) (default . t)))
  (org-preview-latex-image-directory ".ltximg/")
  (org-return-follows-link t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-support-shift-select t)
  (org-tags-column 0)
  (org-todo-keywords '((sequence "TODO" "WIP" "|" "SKIP" "DONE")))

  :ensure-system-package
  (latex . "sudo apt install -y texlive texlive-latex-extra texlive-lang-cjk texlive-extra-utils texlive-luatex texlive-science")
  (pdfcropmargins . "pip install pdfCropMargins")

  :config
  (defun ok-org-mode-hook ()
    (setq-local fill-column 80)
    (turn-on-visual-line-mode))

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
  (setq org-emphasis-regexp-components
        `(;; pre match
          ,(concat (string ?\[ ?\( ?{)
                   "[:space:][:multibyte:]"
                   (string ?\N{ZERO WIDTH SPACE}
                           ?' ?‘ ?\" ?“
                           ?| ?│
                           ?— ?-))     ;; "-" must be the last char
          ;; post match
          ,(concat (string ?\] ?\) ?}) ;; "]" must be the first char
                   "[:space:][:multibyte:]"
                   (string ?\N{ZERO WIDTH SPACE}
                           ?' ?’ ?\" ?”
                           ?| ?│
                           ?. ?, ?? ?! ?\; ?:
                           ?s      ;; allow use like =def=s
                           ?— ?-)) ;; "-" must be the last char
          "[:space:]"              ;; forbidden border chars
          "."                      ;; body "."
          1))                      ;; max newlines
  ;; See `org-emph-re' and `org-verbatim-re' for the final regexps
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components)

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

(use-package org-indent
  :straight nil
  :after (org)
  :hook (org-mode . (lambda () (org-indent-mode 1)))
  :config
  (advice-add #'org-indent-refresh-maybe :around
              (lambda (fun &rest args)
                "Refresh only when buffer is visible."
                ;; This could speed up org-agenda in some cases.
                (when (get-buffer-window (current-buffer) t)
                  (apply fun args)))))

;; ORG BABEL

(use-package ob-core
  :straight nil
  :after (org)
  :config (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package ob-tangle
  :straight nil
  :after (org)
  :config (add-to-list 'org-babel-tangle-lang-exts '("js" . "js")))

(use-package ob-C
  :straight nil
  :after (org)
  :commands (org-babel-execute:C)
  :config
  (add-to-list 'org-babel-load-languages '(C . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-dot
  :straight nil
  :after (org)
  :commands (org-babel-execute:dot)
  :config
  (add-to-list 'org-babel-load-languages '(dot . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-js
  :straight nil
  :after (org)
  :commands (org-babel-execute:js)
  :config
  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-plantuml
  :straight nil
  :after (org)
  :commands (org-babel-execute:plantuml)
  :config
  (add-to-list 'org-babel-load-languages '(plantuml . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-python
  :straight nil
  :after (org)
  :commands (org-babel-execute:python)
  :custom (org-babel-python-command "~/.pyenv/shims/python")
  :config
  (add-to-list 'org-babel-load-languages '(python . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-shell
  :straight nil
  :after (org)
  :commands (org-babel-execute:bash
             org-babel-execute:shell
             org-babel-expand-body:generic)
  :config
  (add-to-list 'org-babel-load-languages '(shell . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-sql
  :straight nil
  :after (org)
  :commands (org-babel-execute:sql)
  :config
  (add-to-list 'org-babel-load-languages '(sql . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-sqlite
  :straight nil
  :after (org)
  :commands (org-babel-execute:sqlite)
  :config
  (add-to-list 'org-babel-load-languages '(sqlite . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ob-typescript
  :after (org)
  :commands (org-babel-execute:typescript)
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; ORG EXPORT

(use-package ox
  :straight nil
  :after (org)
  :custom
  (org-export-with-broken-links t)
  (org-export-with-section-numbers nil)

  :config
  (require 'ox-gfm)
  (require 'ox-hugo)
  (require 'ox-md)

  ;; SUBSTACK EXPORTER

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
    :translate-alist '((link . ok-org--org-html-link))))

(use-package ox-gfm ;; GitHub-flavored markdown
  :after ox)

(use-package ox-hugo
  :after ox)

(use-package ox-md ;; markdown
  :straight nil
  :after ox)

;; ORG AGENDA

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

;; APPEARANCE

(use-package org-modern
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
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :init (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  :hook
  (org-indent-mode . (lambda()
                ;; See github.com/jdtsmith/org-modern-indent/issues/10
                (require 'org-indent)
                (if org-indent--text-line-prefixes
                    (aset org-indent--text-line-prefixes
                          0 (propertize " " 'face 'org-indent)))))
  :config
  (advice-add #'org-modern-indent--refresh-watch :around
              (lambda (fun beg end &rest r)
                "Refresh only when buffer is visible."
                ;; This could speed up org-agenda in some cases.
                (when (get-buffer-window (current-buffer) t)
                  (apply fun beg end r)))))

(use-package valign
  ;; Pixel-perfect visual alignment for Org and Markdown tables.
  :hook (org-mode . valign--maybe-activate)
  :custom
  (valign-fancy-bar t)
  (valign-max-table-size 4000)
  (valign-signal-parse-error t)

  :config
  (defvar ok-org--valign-max-buffer-size 100000
    "Default max-buffer-size above which `valign-mode' will not activate.")

  (defun valign--maybe-activate ()
    (when (<= (buffer-size) ok-org--valign-max-buffer-size)
      (valign-mode 1))))

;; MISC.

(use-package org-contrib)
(use-package org-side-tree :disabled)
(use-package org-web-tools :disabled)

(use-package org-transclusion
  :straight
  (;; Info manual isn't generated by default, so do it here:
   :pre-build
   (("make" "org-transclusion.org")
    ("make" "-C" "./docs" "org-transclusion.texi")
    ("makeinfo" "./docs/org-transclusion.texi" "-o" "./docs/org-transclusion.info")
    ("install-info" "./docs/org-transclusion.info" "./docs/dir")))
  :bind
  (;; no globals
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

;; Local Variables:
;; nameless-aliases: (("" . "ok-org"))
;; End:
;;; 15-org.el ends here
