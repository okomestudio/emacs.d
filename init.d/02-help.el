;;; 02-help.el --- help  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure help and related utilities.
;;
;;; Code:

(use-package helpful
  :bind
  (("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)
   ("C-h f" . helpful-callable) ;; for both functions and macros
   ("C-h k" . helpful-key)
   ("C-h l" . view-lossage)
   ("C-h v" . helpful-variable)
   ("C-h x" . helpful-command)))


(use-package apropos
  :straight nil

  :bind
  (;
   :prefix "C-h a"
   :prefix-map apropos-prefix-map
   ("a" . apropos)
   ("d" . apropos-documentation)
   ("f" . apropos-command)
   ("i" . info-apropos)
   ("l" . apropos-library)
   ("v" . apropos-variable)
   ("C-v" . apropos-value))

  :custom
  (apropos-sort-by-scores t))


(use-package devdocs
  ;; Emacs viewer for DevDocs. See https://devdocs.io.
  ;;
  ;; Run devdocs-install to download select docs locally.
  ;;
  :bind (("C-h D" . devdocs-lookup))
  :hook ((ansible
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
          sql-mode
          web-mode) . devdocs-set-current-docs-for-mode)
  :config
  (setq-default devdocs-current-docs-for-mode
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
                  (sql-mode . ("postgresql~16" "sqlite"))
                  (typescript-ts-mode
                   . ("javascript" "axios" "typescript" "vite"))
                  (web-mode . ("css" "html" "javascript" "react" "typescript"))))

  (defun devdocs-set-current-docs-for-mode ()
    "Set `devdocs-current-docs' for current major mode."
    (setq-local devdocs-current-docs
                (cdr (assoc major-mode devdocs-current-docs-for-mode)))))


(use-package help-shortdoc-example
  ;; Display shortdoc examples to *Help* buffer.
  :straight (:host github :repo "buzztaiki/help-shortdoc-example.el")
  :config (help-shortdoc-example-mode 1))


(use-package hydra)


(use-package which-key
  ;; Displays available keybindings in popup.
  :bind
  (("C-h a k" . which-key-show-top-level))
  :custom
  (which-key-idle-delay 0.5)
  (which-key-max-description-length nil)
  (which-key-min-column-description-width 0)
  (which-key-popup-type 'side-window)
  (which-key-show-docstrings t)
  (which-key-show-transient-maps t)
  (which-key-side-window-location '(right bottom))
  (which-key-side-window-max-width 0.8)
  (which-key-sort-order 'which-key-key-order-alpha)
  :init (which-key-mode +1))


(use-package shell-help
  ;; Show `command -h' help
  :straight nil
  :commands (shell-help)
  :init
  (defun shell-help (str)
    (interactive (list (okutil-string-from-region-or-prompt "Shell command: ")))
    (let ((buffer-stdout "*shell-help*")
          (buffer-stderr "*shell-help-error*")
          (cmd (string-join `(,str "-h") " ")))
      (shell-command cmd buffer-stdout buffer-stderr)
      (switch-to-buffer buffer-stdout))))


(use-package sicp
  ;; "Structure and Interpretation of Computer Programs" as info
  )


(use-package help ;; help-mode
  :straight nil
  :custom (list-faces-sample-text "abcdefghijklmn ABCDEFGHIJKLMN 漢字 ひらがな カタカナ")
  :hook
  (help-mode . (lambda ()
                 (when (member (buffer-name (current-buffer))
                               '("*Colors*" "*Faces*"))
                   ;; `list-*' buffers appear to interfere with `font-lock-mode'
                   (font-lock-mode -1)))))

;;; 02-help.el ends here
