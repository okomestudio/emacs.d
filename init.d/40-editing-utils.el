;;; 40-editing-utils.el --- Editing  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hippie-exp
  ;; Expand the word before the point in various ways.
  :defer t
  :ensure nil

  :init
  (global-set-key [remap dabbrev-expand] 'hippie-expand))


(use-package mwim
  ;; Switch between the beginning/end of line or code line positioning.
  :bind
  (("C-a" . 'mwim-beginning)
   ("C-e" . 'mwim-end)))


(use-package multiple-cursors
  :defer t

  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this))


(use-package htmlize
  ;; Convert buffer text and decorations to HTML
  :defer t
  )


(use-package titlecase
  ;; Titlecase things.
  :defer t

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
  :hook
  (text-mode . typo-mode))


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

  :custom
  (undo-tree-auto-save-history nil)

  :init
  (global-undo-tree-mode))


(use-package whole-line-or-region
  ;; Operate on current line if region undefined.
  )


;;; SPELLING

(use-package flyspell
  :bind
  (:map text-mode-map
   ("M-s M-s" . flyspell-auto-correct-previous-word))

  :hook
  ((prog-mode . flyspell-prog-mode)
   (shell-script-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))


(use-package ispell
  :defer t

  :custom
  (ispell-dictionary "en_US")
  (ispell-local-dictionary-alist
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
     ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB") nil utf-8)))

  :preface
  (put 'ispell-dictionary 'safe-local-variable #'stringp))


;;; SEARCH AND MOVEMENT

(use-package ace-isearch
  ;; A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
  :defer t

  :custom
  (ace-isearch-input-length 1)
  (ace-isearch-jump-delay 0.75)

  :preface
  (defun init-editing--ace-isearch-function-from-isearch ()
    (consult-line isearch-string))

  (defun init-editing--init-ace-isearch ()
    (global-ace-isearch-mode +1)
    ;; This needs to be set after mode activation to override auto-detection:
    (setq ace-isearch-function-from-isearch 'init-editing--ace-isearch-function-from-isearch))

  :init
  (use-package ace-jump-mode)
  (add-hook 'after-init-hook 'init-editing--init-ace-isearch))

;;; 40-editing-utils.el ends here
