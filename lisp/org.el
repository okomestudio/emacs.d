;;; org.el --- Org Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; `org-mode' initialization.
;;
;;; Code:

(use-package org
  :bind (;
         :map org-mode-map
         ("C-c C-l" . org-insert-link)
         ("C-c C-q" . org-set-tags-command)
         ("C-c C-x C-l" . math-preview-all)
         ("C-c i h" . org-insert-heading)  ; or use M-RET
         ("C-c i l" . ok-org-insert-item)
         ("C-c i u" . org-cliplink)
         ("C-c l" . org-store-link)
         ("M-g i" . consult-org-heading))
  :hook (org-mode . ok-org--init-visuals)
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

  :preface
  (ok-safe-local-variable-add org-tags-exclude-from-inheritance listp)

  :config
  ;; ENHANCE DEFAULT BEHAVIORS
  (defun org-open-at-point--ad-prefix (orig-func &optional arg)
    (pcase (car arg)
      ;; With a prefix argument, open the linked file in the same window
      (4 (let ((org-link-frame-setup
                `((file . find-file) . ,org-link-frame-setup)))
           (funcall orig-func arg)))
      (_ (funcall orig-func arg))))

  (advice-add #'org-open-at-point :around #'org-open-at-point--ad-prefix)

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

  ;; EMPHASIS
  (let ((regexp-components
         `(;; pre match
           ,(concat (string ?\[ ?\( ?{)
                    "[:space:][:multibyte:]"
                    (string ?\N{ZERO WIDTH SPACE}
                            ?' ?‘ ?\" ?“
                            ?| ?│
                            ?— ?-))      ; "-" must be the last char
           ;; post match
           ,(concat (string ?\] ?\) ?})  ; "]" must be the first char
                    "[:space:][:multibyte:]"
                    (string ?\N{ZERO WIDTH SPACE}
                            ?' ?’ ?\" ?”
                            ?| ?│
                            ?. ?, ?? ?! ?\; ?:
                            ?s       ; allow use like =def=s
                            ?— ?-))  ; "-" must be the last char
           "[:space:]"               ; forbidden border chars
           "."                       ; body "."
           1)))                      ; max newlines
    ;; See `org-emph-re' and `org-verbatim-re' for the final regexps
    (org-set-emph-re 'org-emphasis-regexp-components regexp-components)))

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
  :custom ((org-export-with-broken-links t)
           (org-export-with-section-numbers nil))
  :config
  (require 'ox-gfm)
  (require 'ox-hugo)
  (require 'ox-md)
  (require 'orp-ok-ox-substack))

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
  (use-package ok
    :straight (:host github :repo "okomestudio/ok.el"))
  (use-package org-modern-indent
    :straight (:host github :repo "jdtsmith/org-modern-indent"))
  (require 'org-theme-ok))

;; MISC.

(use-package org-contrib)

(use-package org-cliplink)
(use-package org-side-tree :disabled)
(use-package org-web-tools :disabled)

(use-package org-transclusion
  :bind (;
         :map org-mode-map
         :prefix "C-c C-n"
         :prefix-map ok-org-transclusion-map
         ("A" . org-transclusion-add-all)
         ("D" . org-transclusion-remove-all)
         ("a" . org-transclusion-add)
         ("t" . org-transclusion-mode))

  ;; NOTE(2024-07-12): The doc generation issue may be fixed at this
  ;; point. Confirm that it works without manual generation and remove
  ;; the following then.
  ;;
  ;; :straight
  ;; (;; Info manual isn't generated by default, so do it here:
  ;;  :pre-build
  ;;  (("make" "org-transclusion.org")
  ;;   ("make" "-C" "./docs" "org-transclusion.texi")
  ;;   ("makeinfo" "./docs/org-transclusion.texi" "-o" "./docs/org-transclusion.info")
  ;;   ("install-info" "./docs/org-transclusion.info" "./docs/dir")))
  ;;
  ;; :init
  ;; (add-to-list 'Info-directory-list
  ;;              (expand-file-name "straight/build/org-transclusion/docs/"
  ;;                                user-emacs-directory))

  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode))

;;; org.el ends here
