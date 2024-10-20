;;; help.el --- help  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Help and documentations.
;;
;;; Code:

(require 'ok)

(use-package help
  :straight nil
  :bind (:prefix
         "C-h w"
         :prefix-map where-or-which-map
         ("i" . where-is))
  :custom (list-faces-sample-text (concat "abcdefghijklmn"
                                          "ABCDEFGHIJKLMN"
                                          "漢字 ひらがな カタカナ"))
  :hook (help-mode . help-ok--disable-font-lock-mode-in-some-buffers)
  :config
  (defun help-ok--disable-font-lock-mode-in-some-buffers ()
    (when (member (buffer-name (current-buffer))
                  '("*Colors*" "*Faces*"))
      ;; `list-*' buffers appear to interfere with `font-lock-mode'
      (font-lock-mode -1))))

(use-package helpful
  :bind (("C-c C-d" . helpful-at-point)
         :map help-map
         ("F" . helpful-function)
         ("f" . helpful-callable)  ; for both functions and macros
         ("k" . helpful-key)
         ("l" . view-lossage)
         ("v" . helpful-variable)
         ("x" . helpful-command)))

(use-package apropos
  :straight nil
  :bind (:prefix
         "C-h a"
         :prefix-map apropos-prefix-map
         ("a" . apropos)
         ("d" . apropos-documentation)
         ("f" . apropos-command)
         ("i" . info-apropos)
         ("l" . apropos-library)
         ("v" . apropos-variable)
         ("C-v" . apropos-value))
  :custom (apropos-sort-by-scores t))

(use-package which-key
  ;; Displays available keybindings in popup.
  :bind ((:map
          where-or-which-map
          ("a" . which-key-show-top-level)))
  :hook (on-first-input . which-key-mode)
  :custom ((which-key-idle-delay 0.5)
           (which-key-idle-secondary-delay 0.05)
           (which-key-max-description-length nil)
           (which-key-min-column-description-width 0)
           (which-key-popup-type 'side-window)
           (which-key-sort-order 'which-key-key-order-alpha)

           ;; NOTE(2024-07-12): The following are disabled for now,
           ;; since `which-key--update' consistently fails due to
           ;; `(wrong-type-argument wholenump ...)'

           ;; (which-key-show-docstrings t)
           ;; (which-key-show-transient-maps t)
           ;; (which-key-side-window-location '(right bottom))
           ;; (which-key-side-window-location 'right)
           ;; (which-key-side-window-max-width 0.8)
           ))

(use-package casual-info
  ;; Provide a keyboard-driven menu UI for the Info reader.
  :bind (:map
         Info-mode-map
         ("C-/" . casual-info-tmenu))
  :custom (casual-info-use-unicode-symbols t))

(use-package help-shortdoc-example
  ;; Display shortdoc examples to *Help* buffer.
  :straight (:host github :repo "buzztaiki/help-shortdoc-example.el")
  :config (help-shortdoc-example-mode 1))

(use-package hydra)

(use-package shell-help
  ;; Show `<command> --help' help.
  :straight nil
  :commands (shell-help)
  :init
  (defun shell-help (str)
    (interactive (list (ok-prompt-or-string-from-region "Shell command: ")))
    (let ((buffer-stdout "*shell-help*")
          (buffer-stderr "*shell-help-error*")
          (cmd (string-join `(,str "-h") " ")))
      (shell-command cmd buffer-stdout buffer-stderr)
      (switch-to-buffer buffer-stdout))))

(use-package help
  :straight nil
  :custom (list-faces-sample-text (concat "abcdefghijklmn"
                                          "ABCDEFGHIJKLMN"
                                          "漢字 ひらがな カタカナ"))
  :hook (help-mode . help--disable-font-lock-mode-in-some-buffers)
  :config
  (defun help--disable-font-lock-mode-in-some-buffers ()
    (when (member (buffer-name (current-buffer))
                  '("*Colors*" "*Faces*"))
      ;; `list-*' buffers appear to interfere with `font-lock-mode'
      (font-lock-mode -1))))

(use-package casual-info
  ;; Provide a keyboard-driven menu UI for the Info reader.
  :bind (;
         :map Info-mode-map
         ("C-/" . casual-info-tmenu))
  :custom (casual-info-use-unicode-symbols t))

;;; Documentations

(use-package devdocs
  ;; Emacs viewer for DevDocs. See https://devdocs.io.
  ;;
  ;; Run `devdocs-install' to download documents on select topics.
  ;;
  :bind (("C-h D" . devdocs-lookup))
  :hook (((ansible
           bash-ts-mode
           css-ts-mode
           dockerfile-ts-mode
           emacs-lisp-mode
           js-jsx-mode
           js-ts-mode
           json-ts-mode
           lisp-data-mode
           magit-mode
           markdown-mode
           mhtml-mode
           python-mode
           python-ts-mode
           web-mode) . devdocs--set-current-docs-for-mode)
         ((sql-mode
           hack-local-variables) . devdocs--set-current-docs-for-sql-mode))
  :config
  (setq-default
   devdocs-current-docs-for-mode
   '((ansible . ("ansible"))
     (bash-ts-mode . ("bash"))
     (css-ts-mode . ("css"))
     (dockerfile-ts-mode . ("docker"))
     (emacs-lisp-mode . ("lisp"))
     (js-jsx-mode . ("javascript" "axios" "react"))
     (js-ts-mode . ("javascript"))
     (json-ts-mode . ("jq"))
     (lisp-data-mode . ("lisp"))
     (magit-mode . ("git"))
     (markdown-mode . ("markdown"))
     (mhtml-mode . ("html"))
     (python-mode . ("python~3.12"))
     (python-ts-mode . ("python~3.12"))
     (typescript-ts-mode . ("javascript" "axios" "typescript" "vite"))
     (web-mode . ("css" "html" "javascript" "react" "typescript"))))

  (defun devdocs--set-current-docs-for-mode ()
    "Set `devdocs-current-docs' for current major mode."
    (setq-local devdocs-current-docs
                (cdr (assoc major-mode devdocs-current-docs-for-mode))))

  (defun devdocs--set-current-docs-for-sql-mode ()
    "Set `devdocs-current-docs' for SQL modes."
    (when (derived-mode-p 'sql-mode)
      (setq-local devdocs-current-docs
                  (pcase sql-product
                    ('postgres '("postgresql~16"))
                    ('sqlite '("sqlite"))
                    (_ '("postgresql~16" "sqlite")))))))

(use-package sicp
  ;; "Structure and Interpretation of Computer Programs" as info.
  )

;;; help.el ends here
