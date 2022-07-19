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
  :disabled
  :hook (after-init-hook . org-roam-mode)
  :custom
  ((org-roam-db-location (file-truename "~/github.com/okomestudio/docs/.org-roam.db"))
   (org-roam-directory (file-truename "~/github.com/okomestudio/docs/"))))

(use-package org-sticky-header
  :init (org-sticky-header-mode +1))

(use-package org-superstar
  :custom (org-superstar-headline-bullets-list '("◉" "🞛" "○" "▷")))

(use-package ob-typescript)
(use-package ox-gfm)

(provide 'init-org)
;;; init-org.el ends here
