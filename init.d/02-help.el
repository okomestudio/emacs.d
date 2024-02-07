;;; 02-help.el --- Help  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Help related utilities.
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
  :bind
  (("C-h D" . devdocs-lookup)))


(use-package help-shortdoc-example
  ;; Display shortdoc examples to *Help* buffer.
  :defer t
  :straight
  (:host github :repo "buzztaiki/help-shortdoc-example.el")

  :config
  (help-shortdoc-example-mode 1))


(use-package which-key
  ;; Displays available keybindings in popup.
  :defer t

  :bind
  (("C-h W" . which-key-show-top-level))

  :custom
  (which-key-idle-delay 0.5)
  (which-key-max-description-length nil)
  (which-key-min-column-description-width 0)
  (which-key-popup-type 'side-window)
  (which-key-show-docstrings nil)
  (which-key-side-window-location '(right bottom))
  (which-key-side-window-max-width 0.8)

  :init
  (which-key-mode +1))


(use-package sicp
  ;; "Structure and Interpretation of Computer Programs" as info
  :defer t)

;;; 02-help.el ends here
