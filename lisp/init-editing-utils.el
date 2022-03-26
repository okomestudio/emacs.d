;;; init-editing-utils.el --- Editing-Utils  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; hippie-expand -- Expand the word before the point in various ways
(use-package hippie-exp
  :ensure nil
  :init
  (global-set-key [remap dabbrev-expand] 'hippie-expand))

;; Switch between the beginning/end of line or codeLine positioning
(use-package mwim
  :bind
  (("C-a" . 'mwim-beginning)
   ("C-e" . 'mwim-end)))

;; multiple-cursors.el - Multiple cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-c C-<") 'mc/mark-all-like-this))

;; Convert buffer text and decorations to HTML
(use-package htmlize)

;; Titlecase things in Emacs
;;
;; https://github.com/duckwork/titlecase.el
(use-package titlecase
  :ensure nil

  :bind
  (("M-c" . titlecase-dwim))

  :custom
  ((titlecase-skip-words-regexps
    '("\\b[[:upper:]]+\\b"
     "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
     )))

  :init
  (ensure-file-from-github "okomestudio/titlecase.el/ts/add-closing-single-quote-as-apostrophe/titlecase.el")
  (ensure-file-from-github "okomestudio/titlecase.el/ts/add-closing-single-quote-as-apostrophe/titlecase-data.el"))

;; Typographical utility (e.g., smart quotation)
(use-package typo
  :hook ((text-mode . typo-mode)))

;; Treat undo history as a tree
(use-package undo-tree
  :custom  (undo-tree-auto-save-history nil)
  :init (global-undo-tree-mode))

;; Operate on current line if region undefined
(use-package whole-line-or-region)


;;; SYNTAX CHECKING

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-pos-tip
  :custom (flycheck-pos-tip-timeout 60)
  :init (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))


;;; SPELLING

(use-package flyspell
  :bind (("M-s M-s" . flyspell-auto-correct-previous-word))
  :hook
  ((prog-mode . flyspell-prog-mode)
   (shell-script-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode)))

(use-package ispell
  :custom
  ((ispell-dictionary "en_US")
   (ispell-local-dictionary-alist
    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
      ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB") nil utf-8))))

  :config
  (put 'ispell-dictionary 'safe-local-variable #'stringp))


;;; CHARACTERS

;; list-unicode-display - Search for and list unicode characters
;; https://github.com/purcell/list-unicode-display
(use-package list-unicode-display)


;;; DICTIONARY

(use-package define-word
  :bind (("M-W" . define-word-at-point))
  :custom (define-word-default-service 'wordnik))


;;; THESAURUS

;; powerthesaurus.el - Powerthesaurus integration for Emacs
;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus
(use-package powerthesaurus
  :bind (("M-s M-S" . powerthesaurus-lookup-dwim)))

;; synosaurus.el - An extensible thesaurus mode for emacs
;; https://github.com/hpdeifel/synosaurus
(use-package synosaurus
  :disabled
  :custom
  (synosaurus-choose-method 'default)

  :ensure-system-package
  (wn . "sudo apt install -y wordnet"))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
