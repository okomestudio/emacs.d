;;; subsys-editing.el --- Editing Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the editing subsystem.
;;
;;; Code:

(use-package crux
  ;; A Collection of Ridiculously Useful eXtensions for Emacs.
  :bind ( ([remap kill-whole-line] . crux-kill-whole-line)
          ("C-S-o". crux-smart-open-line-above)
          ("C-o". crux-smart-open-line) ))

(use-package multiple-cursors
  :bind ( ("C-S-c C-S-c" . mc/edit-lines)
          ("C->" . mc/mark-next-symbol-like-this)
          ("C-<" . mc/mark-previous-symbol-like-this)
          ("C-S-c C-<" . mc/mark-all-like-this) ))

(use-package mwim
  ;; Switch between the beginning/end of line or code line positioning.
  :bind ( ("C-a" . mwim-beginning)
          ("C-e" . mwim-end) ))

(use-package ok
  :bind ( ("C-c i SPC" . ok-edit-insert-zero-width-space)
          ("C-c i s" . ok-edit-insert-section-delimiter)
          ("M-q" . ok-edit-fill-or-unfill-paragraph) ; TODO: Use `prog-fill-reindent-defun'?
          ("M-u" . ok-upcase-word)

          :map prog-mode-map
          ("C-M-;" . ok-edit-align-comments) )
  :config
  (which-key-add-key-based-replacements "C-c i" "ok-edit"))

(use-package titlecase
  ;; Titlecase things.
  :bind ( :map text-mode-map
          ("M-c" . titlecase-dwim)
          :map prog-mode-map
          ("M-c" . titlecase-dwim) )
  :custom
  (titlecase-skip-words-regexps
   '("\\b[[:upper:]]+\\b"
     "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
     )))

(use-package typo
  ;; Typographical editing utility and smart quotation.
  ;;
  ;; The following characters trigger the activation:
  ;;
  ;;   "  `typo-insert-quotation-mark'
  ;;   '  `typo-cycle-right-single-quotation-mark'
  ;;   -  `typo-cycle-dashes'
  ;;   .  `typo-cycle-ellipsis'
  ;;   <  `typo-cycle-left-angle-brackets'
  ;;   >  `typo-cycle-right-angle-brackets'
  ;;   `  `typo-cycle-left-single-quotation-mark'
  ;;
  :hook (org-mode . typo-mode)
  :config
  (define-typo-cycle typo-cycle-dashes
    "Cycle through various dashes."
    ("-"                      ; HYPHEN-MINUS
     "⸺"                      ; TWO-EM DASH (added)
     "―"                      ; HORIZONTAL BAR (added)
     "–"                      ; EN DASH
     "—"                      ; EM DASH
     "−"                      ; MINUS SIGN
     "‐"                      ; HYPHEN
     "‑"                      ; NON-BREAKING HYPHEN
     ))
  (define-typo-cycle typo-cycle-left-angle-brackets
    "Cycle through the less-than sign and guillemet quotation marks."
    ("<" "«" "‹" "<<"))
  (define-typo-cycle typo-cycle-right-angle-brackets
    "Cycle through the greater-than sign and guillemet quotation marks."
    (">" "»" "›" ">>")))

(use-package whole-line-or-region
  ;; Operate on current line if region undefined.
  )

;;; Undo & Redo

(use-package point-undo
  :bind ( ("M-[" . point-undo)
          ("M-]" . point-redo) ))

(use-package undo-fu-session
  :hook (on-first-input . undo-fu-session-global-mode))

(use-package vundo
  :bind ( ("C-c C-/" . vundo)
          ("C-/" . undo)
          ("C-?" . undo-redo) ))

;;; Macro & Expansions

(use-package hippie-exp
  ;; Expand the word before the point in various ways.
  :bind ([remap dabbrev-expand] . hippie-expand))

;;; Region

(use-package expand-region
  :bind ("C-M-S-SPC" . er/expand-region))

(use-package selected
  :commands selected-minor-mode
  :bind ( :map selected-keymap
          ("q" . selected-off) ))

;;; Kaomoji

(use-package kaomel)

;;; Misc.

(use-package htmlize
  ;; Convert buffer text and decorations to HTML
  )

(provide 'subsys-editing)
;;; subsys-editing.el ends here
