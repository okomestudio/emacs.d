;;; maj-org.el --- Org Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure `org-mode'.
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

          ;; NOTE(2025-12-01): See takaxp.github.io/init.html#orgc99664d5
          ("C-c i h" . org-insert-heading) ; or use M-RET
          ("C-c i l" . org-insert-item)

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

  (require 'math-preview)
  (org-ok-mode 1))

(use-package org-contrib)

(use-package org-ok
  :after (org mulex)
  :demand t
  :bind ( :map org-mode-map
          ("C-c C-M-c" . org-ok-babel-run-pytest))
  :init (use-package mulex)
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
  :after (org)
  :config (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package ob-tangle
  :after (org)
  :config (add-to-list 'org-babel-tangle-lang-exts '("js" . "js")))

(use-package ob-C
  :after (org)
  :commands (org-babel-execute:C)
  :config (add-to-list 'org-babel-load-languages '(C . t)))

(use-package ob-dot
  :after (org)
  :commands (org-babel-execute:dot)
  :config (add-to-list 'org-babel-load-languages '(dot . t)))

(use-package ob-js
  :after (org)
  :commands (org-babel-execute:js)
  :config (add-to-list 'org-babel-load-languages '(js . t)))

(use-package ob-plantuml
  :after (org)
  :commands (org-babel-execute:plantuml)
  :config (add-to-list 'org-babel-load-languages '(plantuml . t)))

(use-package ob-python
  :after (org)
  :commands (org-babel-execute:python)
  :custom (org-babel-python-command "~/.pyenv/shims/python")
  :config (add-to-list 'org-babel-load-languages '(python . t)))

(use-package ob-shell
  :after (org)
  :commands (org-babel-execute:bash
             org-babel-execute:shell
             org-babel-expand-body:generic)
  :config (add-to-list 'org-babel-load-languages '(shell . t)))

(use-package ob-sql
  :after (org)
  :commands (org-babel-execute:sql)
  :config (add-to-list 'org-babel-load-languages '(sql . t)))

(use-package ob-sqlite
  :after (org)
  :commands (org-babel-execute:sqlite)
  :config (add-to-list 'org-babel-load-languages '(sqlite . t)))

(use-package ob-typescript
  :after (org)
  :commands (org-babel-execute:typescript)
  :config (add-to-list 'org-babel-load-languages '(typescript . t)))

;;; Org Export

(use-package ox-epub :after ox)
(use-package ox-gfm :after ox) ; markdown (GitHub-flavored)
(use-package ox-hugo :after ox)
(use-package ox-md :after ox) ; markdown
(use-package ox-latex
  :after ox
  :custom ((org-latex-pdf-process
            '("lualatex -interaction nonstopmode -shell-escape %f"
              "lualatex -interaction nonstopmode -shell-escape %f"))))

(use-package ox
  :after org
  :custom ((org-export-with-broken-links t)
           (org-export-with-section-numbers nil))
  :config
  (require 'ox-epub)
  (require 'ox-gfm)
  (require 'ox-hugo)
  (require 'ox-md))

(use-package org-preview-html)

;;; Org Agenda

(use-package org-agenda
  :after (org)
  :custom ((org-agenda-current-time-string "⭠ NOW ────────────────────")
           (org-agenda-include-diary t)
           (org-agenda-inhibit-startup t)
           (org-agenda-skip-unavailable-files t)
           (org-agenda-start-on-weekday 0)
           (org-agenda-use-tag-inheritance t))) ; set nil to speed up parsing

;;; Appearance & Theme

(use-package org-expose-emphasis-markers
  :hook (org-mode . org-expose-emphasis-markers-mode))

(use-package org-modern)

(use-package org-modern-indent)

(use-package org-hide-drawers
  :config
  (defun org-hide-drawers--fold-toggle (fun &rest r)
    (let (result)
      (setq result (apply fun r))
      (if (save-excursion
            (org-fold-folded-p (end-of-line) 'drawer))
          (org-hide-drawers-collapse)
        (org-hide-drawers-expand))
      result))

  (advice-add #'org-fold-hide-drawer-toggle :around
              #'org-hide-drawers--fold-toggle)

  (defun org-hide-drawers--cycle (state)
    (pcase state
      ('subtree (org-hide-drawers-expand))
      ('folded (org-hide-drawers-collapse))))

  (add-hook 'org-cycle-hook #'org-hide-drawers--cycle)

  (defun org-hide-drawers--before-first-heading-p ()
    (if-let* ((heading (org-element-lineage (org-element-at-point)
                                            'headline
                                            'with-self)))
        nil
      t))

  (defun org-hide-drawers-collapse ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-restriction
        (if (org-hide-drawers--before-first-heading-p)
            (narrow-to-defun)
          (org-narrow-to-subtree))
        (org-hide-drawers-make-overlays))))

  (defun org-hide-drawers-expand ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-restriction
        (if (org-hide-drawers--before-first-heading-p)
            (narrow-to-defun)
          (org-narrow-to-subtree))
        (org-hide-drawers-delete-overlays)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward org-property-drawer-re nil t)
            (org-flag-drawer nil))))))

  (defun org-hide-drawers-visibility-toggle ()
    "Toggle the visibility of drawer at point."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-restriction
        (if (org-hide-drawers--before-first-heading-p)
            (narrow-to-defun)
          (org-narrow-to-subtree))
        (if (org-hide-drawers-get-overlays)
            (org-hide-drawers-expand)
          (org-hide-drawers-collapse)))))

  (defun org-hide-drawers--temporarily-deactivate (fun &rest _rest)
    "Deactivate `org-hide-drawers-mode' temporarily when FUN executes."
    (if (member #'org-hide-drawers-mode org-mode-hook)
        (progn
          (remove-hook 'org-mode-hook #'org-hide-drawers-mode)
          (unwind-protect
              (apply fun _rest)
            (add-hook 'org-mode-hook #'org-hide-drawers-mode)))
      (apply fun _rest)))

  (advice-add #'org-roam-extract-subtree :around
              #'org-hide-drawers--temporarily-deactivate))

(use-package org-dividers
  :hook (org-mode . org-dividers-mode))

;;; Clipboard

(use-package org-cliplink
  :after org-download
  :bind ( :map org-mode-map
          ("C-c c w" . org-ok-clipboard-copy-image)
          ("C-c c y" . org-ok-clipboard-smartyank) )
  :custom ((org-cliplink-max-length nil))
  :config
  (defun org-ok-clipboard--link-is-image (link)
    "Return non-nil if LINK points to an image."
    (string-match-p ".*\\.\\(gif\\|jpe?g\\|png\\|webp\\)$" link))

  (defun org-ok-clipboard-smartyank ()
    "Yank a link with page title or an image from the URL in clipboard.
This dispatches yanking to the following functions based on the clipboard
content:
- `org-cliplink' if it is a URL pointing to a web page
- `org-download-yank' if it is a URL pointing to an image
- `org-download-clipboard' if it is an image binary"
    (interactive)
    (if-let* ((content (org-cliplink-clipboard-content))
              (is-url (string-match-p "^https?:.*" content)))
        (if (org-ok-clipboard--link-is-image content)
            (org-download-yank)
          (org-cliplink))
      (org-download-clipboard (format-time-string "fig-%Y%m%dT%H%M%S.png"))))

  (defun org-ok-clipboard-copy-image (path)
    "Copy the image content to clipboard if path points to an image."
    (interactive
     (list (if (thing-at-point 'url)
               (let ((url (thing-at-point 'url)))
                 (if (string-match-p "^file:" url)
                     (expand-file-name (substring url 5))
                   url))
             (read-file-name "Image: " nil nil t nil
                             #'org-ok-clipboard--link-is-image))))
    (if (not (org-ok-clipboard--link-is-image path))
        (error "Path doesn't point to an image"))
    (if-let*
        ((cmd (if (eq system-type 'gnu/linux)
                  (if (executable-find "wl-copy")
                      (format "wl-copy < %s &" path)
                    (if (executable-find "xclip")
                        (let ((ftype "image/png"))
                          ;; TODO(2025-08-19): Inspect image type.
                          (format "xclip -selection clipboard -t %s -i %s &"
                                  ftype path)))))))
        (unwind-protect
            (shell-command cmd)
          nil)
      (warn "Cannot find clipboard tool"))))

(use-package org-download
  ;; Drag and drop images to Emacs org-mode.
  :custom ((org-download-method 'directory)
           (org-download-image-dir nil)
           (org-download-heading-lvl nil)
           (org-download-timestamp ""))
  :hook (org-mode . org-download-enable))

;;; Misc.

(use-package org-id-ext)
(use-package org-side-tree :disabled)
(use-package org-web-tools :disabled)

(use-package org-transclusion
  :after (org)
  :bind ( :map org-mode-map
          :prefix "C-c C-n"
          :prefix-map ok-org-transclusion-map
          ("A" . org-transclusion-add-all)
          ("D" . org-transclusion-remove-all)
          ("a" . org-transclusion-add)
          ("t" . org-transclusion-mode) )
  :init
  (add-to-list 'Info-directory-list
               (expand-file-name "docs/"
                                 (straight--build-dir "org-transclusion")))
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode)

  ;; TODO(2025-08-02): Patch `org-transclusion-add-org-id' to fix a bug reported
  ;; in the following comment:
  ;;
  ;;   - https://github.com/nobiot/org-transclusion/pull/273#issuecomment-3146612720
  ;;
  ;; The patch allows dedicated target/search option to be honored when it is
  ;; located before the first heading in the transcluded Org document.
  (defun org-transclusion-add-org-id--patch (link plist)
    "Return a list for Org-ID LINK object and PLIST.
Return nil if not found."
    (and-let*
        ((_ (string= "id" (org-element-property :type link)))
         (mkr (ignore-errors (org-transclusion-add-target-marker link)))
         (buf (marker-buffer mkr))
         (_ (buffer-live-p (marker-buffer mkr))))
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char mkr)
         (append '(:tc-type "org-id")
                 (if (and (org-before-first-heading-p)
												  (not (looking-at-p "\\(#\\+name:\\|<<\\)")))
                     (org-transclusion-content-org-filtered
                      nil plist)
                   (org-transclusion-content-org-filtered
                    'only-element plist)))))))
  (advice-add #'org-transclusion-add-org-id :override
              #'org-transclusion-add-org-id--patch))

(load (ok-file-expand-lisp "pkg-anki-editor.el"))

(provide 'maj-org)
;;; maj-org.el ends here
