;;; init-org.el --- Org  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package org
  :after (ob-typescript)
  :ensure org-contrib

  :bind
  (:map org-mode-map
   ("C-c l" . 'org-store-link)
   ("M-g i" . 'consult-org-heading)
   ("M-q" . 'okutil-org-fill-or-unfill-paragraph))

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
                (turn-on-visual-line-mode)))

  :config
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
   '(font-lock-comment-face ((t (:inherit 'fixed-pitch))))

   ;; Headings size
   '(org-document-title ((t (:height 1.24))))
   '(org-level-1 ((t (:inherit outline-1 :height 1.24))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.12))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 0.9))))
   '(org-level-5 ((t (:inherit outline-5 :height 0.9))))
   '(org-level-6 ((t (:inherit outline-6 :height 0.9))))
   '(org-level-7 ((t (:inherit outline-7 :height 0.9))))
   '(org-level-8 ((t (:inherit outline-8 :height 0.9)))))

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


(use-package org-modern
  ;; Modern Org Style.
  :custom
  (org-modern-block-name t) ;; use org-modern-indent
  (org-modern-checkbox '((?X . #("▢𐄂" 0 2 (composition ((2)))))
                         (?- . #("▢–" 0 2 (composition ((2)))))
                         (?\s . #("▢" 0 1 (composition ((1)))))))
  (org-modern-hide-stars 'nil)
  (org-modern-keyword "‣ ")
  (org-modern-priority t)
  (org-modern-star '("◉" "🞛" "○" "▷"))
  (org-modern-statistics t)
  (org-modern-table nil)
  (org-modern-tag t)
  (org-modern-timestamp t)
  (org-modern-todo t)

  :init
  (with-eval-after-load 'org
    (global-org-modern-mode)))


(use-package org-modern-indent
  ;; Modern block styling with org-indent.
  :straight
  (org-modern-indent :type git
                     :host github
                     :repo "jdtsmith/org-modern-indent")

  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))


(use-package org-side-tree)


(use-package org-transclusion
  :straight
  (;; Info manual isn't generated by default, so do it here:
   :pre-build
   (("make" "org-transclusion.org")
    ("make" "-C" "./docs" "org-transclusion.texi")
    ("makeinfo" "./docs/org-transclusion.texi" "-o" "./docs/org-transclusion.info")
    ("install-info" "./docs/org-transclusion.info" "./docs/dir")))

  :bind
  (:map org-mode-map
   ("C-c C-n a" . org-transclusion-add)
   ("C-c C-n t" . org-transclusion-mode))

  :init
  (add-to-list 'Info-directory-list
               "/home/taro/.config/emacs/straight/build/org-transclusion/docs/")

  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode))


(use-package org-web-tools)
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


(provide 'init-org)
;;; init-org.el ends here
