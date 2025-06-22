;;; subsys-org.el --- Org  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Org subsystem.
;;
;;; Code:

(require 'dash)
(require 'ok)

(use-package org
  :bind ( :map org-mode-map
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
          ("e" . org-entities-help) )
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
           (org-todo-keywords '((sequence "TODO" "WIP" "|" "SKIP" "DONE")))
           (org-use-speed-commands t))
  :config
  ;; Unset the keybindings to disable org-agenda shortcuts and
  ;; fallback to popper:
  (keymap-unset org-mode-map "C-'" t)
  (keymap-unset org-mode-map "C-S-'" t)
  (keymap-unset org-mode-map "C-M-'" t)

  (defun org-ok-insert-newline-above (&optional arg)
    "Insert a newline before the current line or the current heading.
Without the prefix argument, a newline is inserted before the current
line. With the prefix argument, a newline is inserted before the heading
of the current section."
    (interactive "P")
    (call-interactively (pcase (car arg)
                          (4 (lambda ()
                               (interactive)
                               (save-mark-and-excursion
                                 (org-previous-visible-heading 1)
                                 (org-return))))
                          (_ #'ok-edit-insert-newline-above))))

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

  (require 'math-preview)
  (org-ok-mode 1))

(use-package org-contrib)

(use-package org-ok
  :straight (org-ok :host github
                    :repo "okomestudio/org-ok"
                    :branch "master"
                    :files (:defaults "extensions/*"))
  :after org
  :demand t
  :bind ( :map org-mode-map
          ("C-c C-M-c" . org-ok-babel-run-pytest))
  :config
  (with-eval-after-load 'ox
    (require 'ox-substack)))

(use-package math-preview
  :custom (math-preview-scale 0.8)
  :commands (math-preview-all math-preview-ok-toggle)
  :ensure-system-package
  (math-preview . "npm install -g git+https://gitlab.com/matsievskiysv/math-preview")
  :config
  (defun math-preview-ok-toggle (&optional arg)
    (interactive "P")
    (call-interactively
     (pcase arg
       ('(4) #'math-preview-clear-all)
       (_ #'math-preview-all)))))

;;; Org Babel

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

;;; Org Export

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

(use-package org-preview-html)

;;; Org Agenda

(use-package org-agenda
  :straight nil
  :after (org)
  :custom ((org-agenda-current-time-string "⭠ NOW ────────────────────")
           (org-agenda-include-diary t)
           (org-agenda-inhibit-startup t)
           (org-agenda-start-on-weekday 0)
           (org-agenda-use-tag-inheritance t))) ; set nil to speed up parsing

;;; Appearance & Theme

(use-package org-expose-emphasis-markers
  :hook (org-mode . org-expose-emphasis-markers-mode))

(use-package org-modern)

(use-package org-modern-indent
  ;; NOTE(2025-03-03): Not on any package repository, so need manual
  ;; install here.
  :straight (org-modern-indent :host github :repo "jdtsmith/org-modern-indent"))

(use-package org-hide-drawers
  :straight (org-hide-drawers :host github
                              :repo "krisbalintona/org-hide-drawers"
                              :branch "devel"))

(use-package ok-themes
  :straight (ok-themes :host github :repo "okomestudio/ok-themes.el")
  :demand t
  :init
  (with-eval-after-load 'org
    (load-theme 'ok-org t))

  :config
  (defun ok-org-theme--prepare-fonts (theme)
    "Prepare fonts and fontsets used in the `ok-org' theme."
    (when (eq theme 'ok-org)
      (let* ((fontsets '(( :fontset "fontset-ok org fixed pitch"
                           :font-family "Hack"
                           :subsets ((ja . "BIZ UDGothic")) )
                         ( :fontset "fontset-ok org variable pitch"
                           :font-family "EB Garamond"
                           :subsets ((ja . "Noto Serif CJK JP Medium")) )
                         ( :fontset "fontset-ok org outline"
                           :font-family "URW Classico"
                           :subsets ((ja . "Noto Sans CJK JP"))
                           :char-specs ((?― . "EB Garamond")) ))))
        (dolist (fontset fontsets)
          (ok-fontset-create (plist-get fontset :fontset)
                             (plist-get fontset :font-family)
                             :subsets (plist-get fontset :subsets)
                             :char-specs (plist-get fontset :char-specs)))

        ;; Customize faces
        (set-face-attribute 'ok-org-fixed-pitch nil
                            :family "Hack"
                            :fontset "fontset-fixed pitch")
        (set-face-attribute 'ok-org-variable-pitch nil
                            :family "EB Garamond"
                            :fontset "fontset-variable pitch")
        ;; (set-face-attribute 'ok-org-default nil :family font-family :fontset fontset)
        (set-face-attribute 'ok-org-outline nil
                            :family "URW Classico"
                            :fontset "fontset-ok org outline"))))

  (add-hook 'enable-theme-functions #'ok-org-theme--prepare-fonts -98))

;;; Clipboard

(use-package org-cliplink
  :after org-download
  :bind ( :map org-mode-map
          ("C-c i u" . org-ok-clipboard-smartyank))
  :custom ((org-cliplink-max-length nil))
  :config
  (defun org-ok-clipboard-smartyank ()
    "Yank a link with page title or an image from the URL in clipboard.
This dispatches yanking to the following functions based on the
clipboard content:
- `org-cliplink' if it is a URL pointing to a web page
- `org-download-yank' if it is a URL pointing to an image
- `org-download-clipboard' if it is an image binary"
    (interactive)
    (let ((to-be-yanked (org-cliplink-clipboard-content)))
      (if (string-match-p "^https?:.*" to-be-yanked)
          (if (string-match-p ".*\\.\\(gif\\|jpe?g\\|png\\|webp\\)$" to-be-yanked)
              (org-download-yank)
            (org-cliplink))
        (org-download-clipboard (format-time-string "fig-%Y%m%dT%H%M%S.png"))))))

(use-package org-download
  ;; Drag and drop images to Emacs org-mode.
  :custom ((org-download-method 'directory)
           (org-download-image-dir nil)
           (org-download-heading-lvl nil)
           (org-download-timestamp ""))
  :hook (org-mode . org-download-enable))

;;; Misc.

(use-package org-side-tree :disabled)
(use-package org-web-tools :disabled)

(use-package org-transclusion
  :straight (org-transclusion
             :host github
             :repo "nobiot/org-transclusion" ; "okomestudio/org-transclusion"

             ;; NOTE: Try the feature branch for
             ;; github.com/nobiot/org-transclusion/issues/271
             :branch "feat/transient") ; "id-and-target"
  :after (org)
  :bind ( :map org-mode-map
          :prefix "C-c C-n"
          :prefix-map ok-org-transclusion-map
          ("A" . org-transclusion-add-all)
          ("D" . org-transclusion-remove-all)
          ("a" . org-transclusion-add)
          ("t" . org-transclusion-mode))
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode))

(load (ok-file-expand-lisp "pkg-anki-editor.el"))

(provide 'subsys-org)
;;; subsys-org.el ends here
