;;; 40-editing-utils.el --- Editing  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Text editing utilities.
;;
;;; Code:

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package mwim
  ;; Switch between the beginning/end of line or code line positioning.
  :bind (("C-a" . 'mwim-beginning)
         ("C-e" . 'mwim-end)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-symbol-like-this)
         ("C-<" . mc/mark-previous-symbol-like-this)
         ("C-S-c C-<" . mc/mark-all-like-this)))

(use-package htmlize
  ;; Convert buffer text and decorations to HTML
  )

(use-package ok
  :bind (nil
         :map prog-mode-map
         ("C-M-;" . ok-edit-align-comments)))

(use-package titlecase
  ;; Titlecase things.
  :bind (nil
         :map text-mode-map
         ("M-c" . titlecase-dwim))

  :custom
  (titlecase-skip-words-regexps
   '("\\b[[:upper:]]+\\b"
     "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
     )))

(use-package typo
  ;; Typographical utility (e.g., smart quotation).
  :hook (org-mode . typo-mode))

(use-package undo-tree
  ;; Treat undo history as a tree.
  :bind (;
         :map undo-tree-map
         ("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         :map undo-tree-mode-map
         ("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo))

  :custom (undo-tree-auto-save-history nil)
  :init (global-undo-tree-mode))

(use-package whole-line-or-region
  ;; Operate on current line if region undefined.
  )


;;; SPELLING

(use-package flyspell
  :straight nil
  :bind (;
         :map text-mode-map
         ("M-s M-s" . flyspell-auto-correct-previous-word))

  :hook
  ((prog-mode
    shell-script-mode
    text-mode) . flyspell-prog-mode))

(use-package ispell
  :straight nil
  :custom
  (ispell-dictionary "en_US")
  (ispell-local-dictionary-alist
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
     ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB") nil utf-8)))
  (ispell-program-name "/usr/bin/aspell")

  :preface
  (put 'ispell-dictionary 'safe-local-variable #'stringp))


;;; MACRO AND EXPANSIONS

(use-package hippie-exp
  ;; Expand the word before the point in various ways.
  :straight nil
  :bind ([remap dabbrev-expand] . hippie-expand))

;;; 40-editing-utils.el ends here
