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
  ((fill-column 80)
   (org-adapt-indentation nil)
   (org-agenda-files
    (when (> (length (directory-files default-directory t "\\.org$")) 0)
      (directory-files-recursively default-directory "\\.org$")))
   (org-babel-python-command "~/.pyenv/shims/python")
   (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
   (org-image-actual-width nil)
   (org-list-allow-alphabetical t)
   ;; (org-plantuml-jar-path ts/path-plantuml)
   (org-preview-latex-image-directory ".ltximg/")
   (org-startup-folded t)
   (org-startup-indented t)
   (org-support-shift-select t)
   (org-todo-keywords '((sequence "TODO" "|" "DONE" "SKIP"))))

  :hook
  ((org-mode . (lambda () (org-superstar-mode 1)))
   (org-mode . (lambda () (text-scale-set 1)))
   (org-mode . (lambda () (toggle-truncate-lines -1)))
   (org-mode . auto-fill-mode))

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

  ;; org-confluence-export-as-confluence for Confluence export
  (require 'ox-confluence)

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

  :init
  (defun org-agenda-gather-files ()
    "Gather org agenda files."
    (interactive)
    (setq org-agenda-files (directory-files-recursively default-directory "\\.org$"))))

(use-package org-roam
  :custom
  ((org-roam-db-location (file-truename "~/github.com/okomestudio/docs/.org-roam.db"))
   (org-roam-directory (file-truename "~/github.com/okomestudio/docs/")))

  :hook
  (after-init-hook . org-roam-mode))

(use-package org-sticky-header
  :init (org-sticky-header-mode +1))

(use-package org-superstar
  :custom (org-superstar-headline-bullets-list '("◉" "🞛" "○" "▷")))

(use-package ob-typescript)

(provide 'init-org)
;;; init-org.el ends here
