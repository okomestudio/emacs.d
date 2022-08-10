;;; init-org.el --- Org  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom ts/org-agenda-files "~/.org-agenda-files"
  "Default org-agenda-files."
  :type '(string)
  :group 'ts)

(defcustom ts/org-books-file "~/.books.org"
  "Default org-books-file."
  :type '(string)
  :group 'ts)

(defcustom ts/org-default-notes-file "~/.notes.org"
  "Default org-default-notes-file."
  :type '(string)
  :group 'ts)

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

  (plist-put org-format-latex-options :scale 1.5)

  ;; Add a few characters usable for bounding emphasis markup
  (setcar org-emphasis-regexp-components "-—[:space:]('\"{\x200B")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "-—[:space:].,:!?;'\")}\\[\x200B")
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
   (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
   (org-hide-emphasis-markers t)
   (org-image-actual-width nil)
   ;; (org-indent-indentation-per-level 4)
   (org-list-allow-alphabetical t)
   (org-list-indent-offset 2)
   ;; (org-plantuml-jar-path ts/path-plantuml)
   (org-preview-latex-image-directory ".ltximg/")
   (org-return-follows-link t)
   (org-startup-folded t)
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
  :ensure nil
  :config
  (if (daemonp)
      (add-hook 'emacs-startup-hook
                '(lambda ()
                   ;; First, empty agenda files to successfully create an Org Agenda buffer:
                   (setq org-agenda-files ())
                   (org-agenda nil "a")
                   ;; then, load the full list of agenda files and refresh the buffer:
                   (setq org-agenda-files (ts/org--init-org-agenda-files ts/org-agenda-files))
                   (org-agenda nil "n")
                   ;; This is to avoid "No Org agenda currently displayed" error.
                   ))
    (setq org-agenda-files (ts/org--init-org-agenda-files ts/org-agenda-files)))

  :custom
  (org-agenda-current-time-string "⭠ NOW ────────────────────")
  (org-agenda-include-diary t)
  (org-agenda-start-on-weekday 0)

  :init
  (defun ts/org--init-org-agenda-files (pathlist)
    "Gather agenda files recursively with directories defined in PATHLIST."
    (with-temp-buffer
      (insert-file-contents pathlist)
      (let* (gathered-files '())
        (while (not (eobp))
          (let* ((path (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
            (if (f-directory-p path)
                (setq gathered-files
                      (append gathered-files
                              (directory-files-recursively path "\\.org$" ))))
            (if (f-file-p path)
                (push path gathered-files)))
          (forward-line 1))
        gathered-files))))

;; org-books - Reading list management with org mode
;; https://github.com/lepisma/org-books
(use-package org-books
  :custom (org-books-file ts/org-books-file))

;; org-modern - Modern Org Style
;; https://github.com/minad/org-modern
(use-package org-modern
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

  ;; :hook
  ;; ((org-mode . org-modern-mode))

  :init
  ;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (global-org-modern-mode))

(use-package org-roam
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)

  :config
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE. Overridden to use hyphens instead of underscores."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768      ; U+0300 COMBINING GRAVE ACCENT
                             769      ; U+0301 COMBINING ACUTE ACCENT
                             770      ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771      ; U+0303 COMBINING TILDE
                             772      ; U+0304 COMBINING MACRON
                             774      ; U+0306 COMBINING BREVE
                             775      ; U+0307 COMBINING DOT ABOVE
                             776      ; U+0308 COMBINING DIAERESIS
                             777      ; U+0309 COMBINING HOOK ABOVE
                             778      ; U+030A COMBINING RING ABOVE
                             779      ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780      ; U+030C COMBINING CARON
                             795      ; U+031B COMBINING HORN
                             803      ; U+0323 COMBINING DOT BELOW
                             804      ; U+0324 COMBINING DIAERESIS BELOW
                             805      ; U+0325 COMBINING RING BELOW
                             807      ; U+0327 COMBINING CEDILLA
                             813      ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814      ; U+032E COMBINING BREVE BELOW
                             816      ; U+0330 COMBINING TILDE BELOW
                             817      ; U+0331 COMBINING MACRON BELOW
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
          (downcase slug)))))

  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)

  :custom
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?\n<%<%Y-%m-%d %a %H:%M>>"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-location "~/github.com/okomestudio/docs/roam/.roam.db")
  (org-roam-directory "~/github.com/okomestudio/docs/roam")
  (org-roam-node-display-template "${title} ${tags}")

  :init
  (put 'org-roam-ui-port 'safe-local-variable #'integerp)
  (put 'org-roam-directory 'safe-local-variable #'stringp)
  (put 'org-roam-db-location 'safe-local-variable #'stringp)
  (put 'org-roam-capture-templates 'safe-local-variable #'listp))

(use-package org-roam-ui
  ;; :straight
  ;; (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam

  :custom
  (org-roam-ui-update-on-save t)
  ;; (org-roam-ui-sync-theme t)
  ;; (org-roam-ui-follow t)
  ;; (org-roam-ui-open-on-start t)
  )

(use-package org-sticky-header
  :init (org-sticky-header-mode +1))

(use-package org-superstar
  :custom (org-superstar-headline-bullets-list '("◉" "🞛" "○" "▷")))

(use-package ob-typescript)
(use-package ox-gfm)

(provide 'init-org)
;;; init-org.el ends here
