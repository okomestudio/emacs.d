;;; subsys-help.el --- Help Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the help and documentation subsystem.
;;
;;; Code:

(require 'ok)

;;; Info

(use-package info
  :straight nil
  :custom (Info-hide-note-references t)
  :config (push (ok-file-expand-etc "info/") Info-directory-list))

(use-package sicp
  ;; "Structure and Interpretation of Computer Programs" as info.
  )

(use-package emacs-lisp-elements
  :straight (emacs-lisp-elements :host github
                                 :repo "protesilaos/emacs-lisp-elements"))

;;; Help

(use-package elisp-for-python
  :straight (elisp-for-python :host github
                              :repo "kickingvegas/elisp-for-python"))

(use-package help
  :straight nil
  :bind ( :map help-map
          ("d" . ok-help-shell-cmd )

          :prefix "C-h V"
          :prefix-map help-view-other-doc
          ("p" . help-ok-view-doc-elisp-for-python)
          ("s" . help-ok-view-doc-straight)
          ("q" . help-ok-view-doc-emacsql)
          ("t" . help-ok-view-doc-tempel) )
  :custom (list-faces-sample-text (concat "abcdefghijklmn"
                                          "ABCDEFGHIJKLMN"
                                          "漢字 ひらがな カタカナ"))
  :hook (help-mode . help-ok--disable-font-lock-mode-in-some-buffers)
  :config
  (defun help-ok--disable-font-lock-mode-in-some-buffers ()
    (when (member (buffer-name (current-buffer))
                  '("*Colors*" "*Faces*"))
      ;; `list-*' buffers appear to interfere with `font-lock-mode'
      (font-lock-mode -1)))

  ;; Dynamically generate document accessor functions
  (let ((docs
         `((emacsql
            . ,(ok-file-expand-straight-repos "emacsql" "README.md"))
           (elisp-for-python
            . ,(ok-file-expand-straight-repos "elisp-for-python" "README.org"))
           (straight
            . ,(ok-file-expand-straight-repos "straight.el" "README.md"))
           (tempel
            . ,(ok-file-expand-straight-repos "tempel" "README.org")))))
    (dolist (doc docs)
      (let ((sym (symbol-name (car doc)))
            (file (cdr doc)))
        (defalias (intern (concat "help-ok-view-doc-" sym))
          (lambda ()
            (interactive)
            (if (file-exists-p file)
                (find-file-other-window file)
              (message "File not found: %s" file)))
          (format "View `%s' README." sym))))))

(use-package helpful
  :bind ( ("C-c C-d" . helpful-at-point)
          :map help-map
          ("F" . helpful-function)
          ("f" . helpful-callable)  ; for both functions and macros
          ("k" . helpful-key)
          ("l" . view-lossage)
          ("v" . helpful-variable)
          ("x" . helpful-command) ))

(use-package apropos
  :straight nil
  :bind ( :prefix "C-h a"
          :prefix-map apropos-prefix-map
          ("a" . apropos)
          ("d" . apropos-documentation)
          ("f" . apropos-command)
          ("i" . info-apropos)
          ("l" . apropos-library)
          ("v" . apropos-variable)
          ("C-v" . apropos-value) )
  :custom (apropos-sort-by-scores t))

(use-package help-shortdoc-example
  ;; Display shortdoc examples to *Help* buffer.
  :straight (help-shortdoc-example :host github
                                   :repo "buzztaiki/help-shortdoc-example.el")
  :config (help-shortdoc-example-mode 1))

(use-package man-index
  ;; Quickly navigate to keywords within a man page.
  :straight (man-index :type git :host codeberg :repo "imarko/man-index")
  :after man
  :bind ( :map Man-mode-map
          ("i" . man-index) ))

;;; Which-key

(use-package which-key
  ;; Displays available keybindings in popup.
  :straight nil
  :bind ( ("C-h C-h" . nil) ;; clear, otherwise will bind to `help-for-help'

          :prefix-map where-or-which-map
          :prefix "C-h w"
          ("i" . where-is)

          :map help-map
          ("C-?" . which-key-show-top-level)

          :map which-key-C-h-map
          ("s" . which-key-ok--change-sort-order) )
  :custom ((which-key-idle-delay 0.5)
           (which-key-idle-secondary-delay 0.05)
           (which-key-max-description-length 79)
           (which-key-min-column-description-width 0)
           (which-key-popup-type 'side-window)
           (which-key-separator ": ")

           ;; NOTE(2025-03-05): Use "d" option within the mode buffer
           ;; to toggle docstring. When the window width is small,
           ;; `which-key--update' consistently fails due to an error
           ;; `(wrong-type-argument wholenump ...)'.
           (which-key-show-docstrings nil)

           (which-key-show-transient-maps t)
           (which-key-side-window-location '(right bottom))
           ;; (which-key-side-window-location 'right)
           (which-key-side-window-max-width 0.8)
           (which-key-sort-order 'which-key-description-order))
  :hook (on-first-input . which-key-mode)
  :config
  ;; Enable changing sort order.
  (setq which-key-C-h-map-prompt
        (concat which-key-C-h-map-prompt
                ", \\[which-key-ok--change-sort-order]"
                which-key-separator
                "change sort order"))

  (defun which-key-ok--change-sort-order (&rest _)
    "Toggle `which-key-sort-order'."
    (interactive)
    (setopt which-key-sort-order
            (pcase which-key-sort-order
              ('which-key-description-order 'which-key-key-order)
              (_ 'which-key-description-order)))
    (which-key-reload-key-sequence)
    (which-key--create-buffer-and-show (which-key--current-prefix)))

  (which-key-add-key-based-replacements
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x 8 e" "emoji"
    "C-x 8" "char"
    "C-x C-a" "edebug"
    "C-x RET" "char-coding"
    "C-x X" "edebug"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x p" "project"
    "C-x r" "register/bookmark/rect"
    "C-x t" "tab"
    "C-x w" "window"))

(use-package which-key-posframe
  :custom ((which-key-posframe-parameters '((border-width . 2)
                                            (internal-border-width . 1)
                                            (left-fringe . 20)
                                            (right-fringe . 20))))
  :hook (on-first-input . which-key-posframe-mode))

;;; Transient Utilities

(use-package casual
  ;; Provide a keyboard-driven menu UI
  :bind ( :map Info-mode-map
          ("C-/" . casual-info-tmenu) )
  :custom ((casual-info-use-unicode-symbols t)
           (casual-lib-use-unicode t))
  :config (require 'casual-info))

(use-package hydra)

;;; Keystrokes
;;
;; See emacs.stackexchange.com/a/81581/599 for options:
;;
;; - github.com/chuntaro/emacs-keypression
;; - gitlab.com/marcowahl/keystrokes
;; - www.emacswiki.org/emacs/ShowKey
;; - github.com/tarsius/keycast
;; - github.com/lewang/command-log-mode

(use-package keycast)
(use-package command-log-mode
  :custom ((command-log-mode-auto-show t))
  :demand t)

;;; Devdocs

(use-package devdocs
  ;; Emacs viewer for DevDocs. See https://devdocs.io.
  ;;
  ;; Run `devdocs-install' to download documents on select topics.
  ;;
  :bind ( :map help-map
          ("D" . devdocs-lookup)
          :map devdocs-mode-map
          ("v" . devdocs-ok-visit) )
  :hook (((hack-local-variables
           prog-mode
           special-mode
           text-mode) . devdocs-ok--set-current-docs))
  :config
  (defun devdocs-ok-visit ()
    "Visit the current devdocs page with a web browser."
    (interactive)
    (devdocs-copy-url)
    (browse-url-generic (current-kill 0)))

  (defun devdocs-ok--set-current-docs ()
    "Set `devdocs-current-docs' for current major mode."
    (when-let*
        ((docs
          (pcase major-mode
            ('bash-ts-mode '("bash"))
            ('css-ts-mode '("css"))
            ('dockerfile-ts-mode '("docker"))
            ;; ('emacs-lisp-mode '("elisp")) ; use Info
            ('js-jsx-mode '("javascript" "axios" "react"))
            ('js-ts-mode '("javascript"))
            ('json-ts-mode '("jq"))
            ('magit-mode '("git"))
            ('markdown-mode '("markdown"))
            ('mhtml-mode '("html"))
            ('python-mode '("python~3.12"))
            ('python-ts-mode '("python~3.12"))
            ('sql-mode (pcase sql-product
                         ('postgres '("postgresql~16"))
                         ('sqlite '("sqlite"))
                         (_ '("postgresql~16" "sqlite"))))
            ('typescript-ts-mode '("javascript" "axios" "typescript" "vite"))
            ('web-mode '("css" "html" "javascript" "react" "typescript"))
            ('yaml-ts-mode (when (and (featurep 'ansible) ansible-mode)
                             '("ansible")))
            (_ nil))))
      (set (make-local-variable 'devdocs-current-docs) docs))))

(provide 'subsys-help)
;;; subsys-help.el ends here
