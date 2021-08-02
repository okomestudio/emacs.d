;;; init-org.el --- Org  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ob-typescript)

(use-package org-plus-contrib
  :after
  (plantuml-mode)

  :bind
  (("C-c l" . 'org-store-link))

  :config
  (plist-put org-format-latex-options :scale 1.5)

  ;; Add a few characters usable for bounding emphasis markup
  (setcar org-emphasis-regexp-components "-—[:space:]('\"{\x200B")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "-—[:space:].,:!?;'\")}\\[\x200B")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (add-to-list
   'org-src-lang-modes '("plantuml" . plantuml))

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
   (org-plantuml-jar-path ts/path-plantuml)
   (org-preview-latex-image-directory ".ltximg/")
   (org-startup-folded t)
   (org-support-shift-select t)
   (org-todo-keywords '((sequence "TODO" "|" "DONE" "SKIP"))))

  :hook
  ((org-mode . (lambda () (org-superstar-mode 1)))
   (org-mode . auto-fill-mode)
   (org-mode . org-indent-mode))

  :init
  (defun org-agenda-gather-files ()
    "Gather org agenda files."
    (interactive)
    (setq org-agenda-files (directory-files-recursively default-directory "\\.org$")))

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
     (typescript . t))))

(use-package org-roam
  :custom
  ((org-roam-db-location (file-truename "~/github.com/okomestudio/docs/.org-roam.db"))
   (org-roam-directory (file-truename "~/github.com/okomestudio/docs/")))

  :hook
  (after-init-hook . org-roam-mode))

(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '("◉" "🞛" "○" "▷")))

(provide 'init-org)
;;; init-org.el ends here
