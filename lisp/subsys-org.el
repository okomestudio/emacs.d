;;; subsys-org.el --- Org Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Org subsystem.
;;
;;; Code:

(require 'ok)

(use-package org
  :bind (:map
         org-mode-map
         ("C-S-o" . org-ok-insert-newline-above)
         ("C-c C-l" . org-insert-link)
         ("C-c C-q" . org-set-tags-command)
         ("C-c C-x l" . math-preview-ok-toggle)
         ("C-c i h" . org-insert-heading) ; or use M-RET
         ("C-c i l" . ok-org-insert-item)
         ("C-c i u" . org-cliplink)
         ("C-c l" . org-store-link)
         ("M-g i" . consult-org-heading)
         :prefix-map org-mode-help-map
         :prefix-docstring "Keymap for org-mode help"
         :prefix "C-h O"
         ("e" . org-entities-help))
  :custom ((org-adapt-indentation nil)
           (org-blank-before-new-entry '((heading . nil)
                                         (plain-list-item . nil)))
           (org-ellipsis "⮷")
           (org-file-apps '(("\\.mp4\\'" . "vlc --repeat %s")))
           (org-footnote-section nil)
           (org-hide-emphasis-markers t)
           (org-image-actual-width nil)
           (org-imenu-depth 6)
           (org-list-allow-alphabetical t)
           (org-list-indent-offset 2)
           (org-M-RET-may-split-line '((headline . nil)
                                       (default . t)))
           (org-return-follows-link t)
           (org-startup-folded nil)
           (org-startup-indented t)
           (org-support-shift-select t)
           (org-tags-column 0)
           (org-todo-keywords '((sequence "TODO" "WIP" "|" "SKIP" "DONE"))))
  :hook (org-mode . ok-org--init-visuals)
  :preface (ok-safe-local-variable-add org-tags-exclude-from-inheritance listp)
  :config
  ;; ENHANCE DEFAULT BEHAVIORS
  (defun org-ok-insert-newline-above-heading ()
    "Insert an empty line before the heading of the current section."
    (interactive)
    (save-mark-and-excursion
      (org-previous-visible-heading 1)
      (org-return)))

  (defun org-ok-insert-newline-above (&optional arg)
    "Insert a newline before the current line or the current heading.
Without a prefix argument, a newline is inserted before the
current line. With a prefix argument, a newline is inserted
before the heading of the current section."
    (interactive "P")
    (call-interactively
     (pcase (car arg)
       (4 #'org-ok-insert-newline-above-heading)
       (_ #'ok-edit-insert-newline-above))))

  (defun org-open-at-point--ad-prefix (orig-func &optional arg)
    (pcase (car arg)
      ;; With a prefix argument, open the linked file in the same window
      (4 (let ((org-link-frame-setup
                `((file . find-file) . ,org-link-frame-setup)))
           (funcall orig-func arg)))
      (_ (funcall orig-func arg))))

  (advice-add #'org-open-at-point :around #'org-open-at-point--ad-prefix)

  (defun math-preview-ok-toggle (&optional arg)
    (interactive "P")
    (call-interactively
     (pcase arg
       ('(4) #'math-preview-clear-all)
       (_ #'math-preview-all))))

  ;; HELPER FUNCTIONS
  (defun ok-org--init-visuals ()
    (setq-local fill-column 80)
    (turn-on-visual-line-mode))

  (defun ok-org-insert-item (begin end)
    "See Section 14.44 at https://takaxp.github.io/init.html."
    (interactive "r")
    (unless mark-active
      (setq begin (line-beginning-position))
      (setq end (line-end-position)))
    (let* ((bullet "  - ")
           (len (string-width bullet)))
      (goto-char begin)
      (while (and (re-search-forward (concat "\\(^[ \t]*\\)") end t)
                  (not (looking-at "[-\\+\\*][ \t]\\|[a-z0-9A-Z]*[\\.)][ \t]"))
                  (not (equal (point) end)))
        (replace-match (concat "\\1" bullet) nil nil)
        (setq end (+ end len)))
      (goto-char begin)))

  (org-plugin-ok-mode 1))

(use-package org-plugin-ok
  :straight (:host github :repo "okomestudio/org-plugin-ok"
                   :files (:defaults "extensions/*"))
  :after org
  :demand t
  :bind (:map
         org-mode-map
         ("C-c C-M-c" . op-ok-babel-run-pytest))
  :config
  (with-eval-after-load 'ox
    (require 'ox-substack)))

(use-package math-preview
  :commands (math-preview-all)
  :custom (math-preview-scale 0.8)
  :ensure-system-package
  (math-preview . "npm install -g git+https://gitlab.com/matsievskiysv/math-preview"))

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
  :config (add-to-list 'org-babel-load-languages '(C . t)))

(use-package ob-dot
  :straight nil
  :after (org)
  :commands (org-babel-execute:dot)
  :config (add-to-list 'org-babel-load-languages '(dot . t)))

(use-package ob-js
  :straight nil
  :after (org)
  :commands (org-babel-execute:js)
  :config (add-to-list 'org-babel-load-languages '(js . t)))

(use-package ob-plantuml
  :straight nil
  :after (org)
  :commands (org-babel-execute:plantuml)
  :config (add-to-list 'org-babel-load-languages '(plantuml . t)))

(use-package ob-python
  :straight nil
  :after (org)
  :commands (org-babel-execute:python)
  :custom (org-babel-python-command "~/.pyenv/shims/python")
  :config (add-to-list 'org-babel-load-languages '(python . t)))

(use-package ob-shell
  :straight nil
  :after (org)
  :commands (org-babel-execute:bash
             org-babel-execute:shell
             org-babel-expand-body:generic)
  :config (add-to-list 'org-babel-load-languages '(shell . t)))

(use-package ob-sql
  :straight nil
  :after (org)
  :commands (org-babel-execute:sql)
  :config (add-to-list 'org-babel-load-languages '(sql . t)))

(use-package ob-sqlite
  :straight nil
  :after (org)
  :commands (org-babel-execute:sqlite)
  :config (add-to-list 'org-babel-load-languages '(sqlite . t)))

(use-package ob-typescript
  :after (org)
  :commands (org-babel-execute:typescript)
  :config (add-to-list 'org-babel-load-languages '(typescript . t)))

;; ORG EXPORT

(use-package ox
  :straight nil
  :after (org)
  :custom ((org-export-with-broken-links t)
           (org-export-with-section-numbers nil))
  :config
  (require 'ox-gfm)
  (require 'ox-hugo)
  (require 'ox-md))

(use-package ox-gfm ;; GitHub-flavored markdown
  :after ox)

(use-package ox-hugo
  :after ox)

(use-package ox-latex
  :straight nil
  :after ox
  :custom ((org-latex-pdf-process
            '("lualatex -interaction nonstopmode -shell-escape %f"
              "lualatex -interaction nonstopmode -shell-escape %f"))))

(use-package ox-md ;; markdown
  :straight nil
  :after ox)

;; ORG AGENDA

(use-package org-agenda
  :straight nil
  :after (org)
  :custom ((org-agenda-current-time-string "⭠ NOW ────────────────────")
           (org-agenda-include-diary t)
           (org-agenda-inhibit-startup t)
           (org-agenda-start-on-weekday 0)
           (org-agenda-use-tag-inheritance t))  ; set nil to speed up parsing
  :preface
  (put 'org-agenda-custom-commands 'safe-local-variable #'listp))

;; THEME

(use-package org-theme-ok
  :straight (:host github :repo "okomestudio/org-theme-ok.el")
  :init
  (use-package org-modern-indent
    :straight (:host github :repo "jdtsmith/org-modern-indent"))
  (require 'org-theme-ok))

;; MISC.

(use-package org-contrib)

(use-package org-cliplink)
(use-package org-side-tree :disabled)
(use-package org-web-tools :disabled)

(use-package org-download
  ;; Drag and drop images to Emacs org-mode.
  :bind (:map
         org-mode-map
         ("C-c i c" . org-download-clipboard)
         ("C-c i i" . org-download-yank))
  :custom ((org-download-method 'directory)
           (org-download-image-dir nil)
           (org-download-heading-lvl nil)
           (org-download-timestamp ""))
  :hook (org-mode . org-download-enable))

(use-package org-transclusion
  :bind (:map
         org-mode-map
         :prefix "C-c C-n"
         :prefix-map ok-org-transclusion-map
         ("A" . org-transclusion-add-all)
         ("D" . org-transclusion-remove-all)
         ("a" . org-transclusion-add)
         ("t" . org-transclusion-mode))
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode))

(load (locate-user-emacs-file "lisp/pkg-anki-editor.el"))

(provide 'subsys-org)
;;; subsys-org.el ends here
