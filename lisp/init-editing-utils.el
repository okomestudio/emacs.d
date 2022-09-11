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


;;; SEARCH AND MOVEMENT

;; ace-isearch - A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
;; https://github.com/tam17aki/ace-isearch
(use-package ace-isearch
  :custom
  (ace-isearch-input-length 6)
  (ace-isearch-jump-delay 0.75)

  :init
  (defun ts/ace-isearch-function-from-isearch ()
    (consult-line isearch-string))

  (defun ts/init-ace-isearch ()
    (global-ace-isearch-mode +1)
    ;; This needs to be set after mode activation to override auto-detection:
    (setq ace-isearch-function-from-isearch
          'ts/ace-isearch-function-from-isearch))

  (use-package ace-jump-mode)

  (add-hook 'after-init-hook 'ts/init-ace-isearch))


;;; CHARACTERS

;; list-unicode-display - Search for and list unicode characters
;; https://github.com/purcell/list-unicode-display
(use-package list-unicode-display)


;;; DICTIONARY AND LOOKING UP

(use-package define-word
  :bind (("M-W" . define-word-at-point))
  :custom (define-word-default-service 'wordnik))

;; powerthesaurus.el - Powerthesaurus integration for Emacs
;; https://github.com/SavchenkoValeriy/emacs-powerthesaurus
(use-package powerthesaurus
  :bind (("M-s M-S" . powerthesaurus-lookup-dwim)))

;; synosaurus.el - An extensible thesaurus mode for emacs
;; https://github.com/hpdeifel/synosaurus
(use-package synosaurus
  :disabled
  :custom (synosaurus-choose-method 'default)
  :ensure-system-package (wn . "sudo apt install -y wordnet"))

;; Wrap eww to enable quicker look up in some sites
(use-package eww
  :config

  ;; See http://emacs.rubikitch.com/eww-weblio/ for reference.
  (defun ts/eww-set-start-at (url-regexp search-regexp)
    "When site matches URL-REGEXP, start displaying from line matching SEARCH-REGEXP."
    (when (string-match url-regexp (plist-get eww-data :url))
      (goto-char (point-min))
      (when (re-search-forward search-regexp nil t)
        (recenter 0))))

  (defun ts/eww-render--after (&rest _)
    (ts/eww-set-start-at "amazon.co.jp" "^結果")
    (ts/eww-set-start-at "amazon.com" "^RESULTS")
    (ts/eww-set-start-at "en.m.wikipedia.org" "^ *Search")
    (ts/eww-set-start-at "ja.m.wikipedia.org" "^ *検索")
    (ts/eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書"))
  (add-hook 'eww-after-render-hook 'ts/eww-render--after)

  (defun ts/region-or-read-string (prompt &optional initial history default inherit)
    "When region is specified, use it as string; otherwise, get it interactively."
    (if (not (region-active-p))
        (read-string prompt initial history default inherit)
      (prog1
          (buffer-substring-no-properties (region-beginning) (region-end))
        (deactivate-mark)
        (message ""))))

  (defun ts/make-query (site-url str)
    (eww-browse-url (format site-url (url-hexify-string str))))

  (defun search-amazon (str)
    (interactive (list (ts/region-or-read-string "Amazon (US): ")))
    (ts/make-query "https://amazon.com/s?k=%s" str))

  (defun search-amazon-ja (str)
    (interactive (list (ts/region-or-read-string "Amazon (JP): ")))
    (ts/make-query "https://amazon.co.jp/s?k=%s" str))

  (defun search-weblio (str)
    (interactive (list (ts/region-or-read-string "Weblio: ")))
    (ts/make-query "https://www.weblio.jp/content/%s" (upcase str)))

  (defun search-wikipedia (str)
    (interactive (list (ts/region-or-read-string "Wikipedia (en): ")))
    (ts/make-query "https://en.m.wikipedia.org/wiki/%s" str))

  (defun search-wikipedia-ja (str)
    (interactive (list (ts/region-or-read-string "Wikipedia (ja): ")))
    (ts/make-query "https://ja.m.wikipedia.org/wiki/%s" str)))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
