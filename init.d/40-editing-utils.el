;;; 40-editing-utils.el --- Editing  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hippie-exp
  ;; Expand the word before the point in various ways.
  :straight nil
  :bind
  ([remap dabbrev-expand] . hippie-expand))


(use-package mwim
  ;; Switch between the beginning/end of line or code line positioning.
  :bind
  (("C-a" . 'mwim-beginning)
   ("C-e" . 'mwim-end)))


(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-S-c C-<" . mc/mark-all-like-this))


(use-package htmlize
  ;; Convert buffer text and decorations to HTML
  )


(use-package titlecase
  ;; Titlecase things.
  :bind
  (;
   :map text-mode-map
   ("M-c" . titlecase-dwim))

  :custom
  (titlecase-skip-words-regexps
   '("\\b[[:upper:]]+\\b"
     "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
     )))


(use-package typo
  ;; Typographical utility (e.g., smart quotation).
  :hook (text-mode . typo-mode))


(use-package undo-tree
  ;; Treat undo history as a tree.
  :bind
  (;
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
  :bind
  (;
   :map text-mode-map
   ("M-s M-s" . flyspell-auto-correct-previous-word))

  :hook
  ((prog-mode
    shell-script-mode
    text-mode) . flyspell-prog-mode))


(use-package ispell
  :custom
  (ispell-dictionary "en_US")
  (ispell-local-dictionary-alist
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
     ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB") nil utf-8)))

  :preface
  (put 'ispell-dictionary 'safe-local-variable #'stringp))

;;; 40-editing-utils.el ends here
