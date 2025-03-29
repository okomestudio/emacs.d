;;; subsys-editing.el --- Editing Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the editing subsystem.
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
  :bind ( ("C-S-o" . ok-edit-insert-newline-above)
          ("C-c i SPC" . ok-edit-insert-zero-width-space)
          ("C-c i s" . ok-edit-insert-section-delimiter)
          ("C-o" . ok-edit-insert-newline-below)
          ("M-q" . ok-edit-fill-or-unfill-paragraph) ; TODO: Use `prog-fill-reindent-defun'?

          :map prog-mode-map
          ("C-M-;" . ok-edit-align-comments) )
  :config
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c i" "ok-edit")))

(use-package repeat
  :hook (on-first-input . repeat-mode))

(use-package titlecase
  ;; Titlecase things.
  :bind ( :map text-mode-map
          ("M-c" . titlecase-dwim) )
  :custom
  (titlecase-skip-words-regexps
   '("\\b[[:upper:]]+\\b"
     "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
     )))

(use-package typo
  ;; Typographical utility (e.g., smart quotation).
  :hook (org-mode . typo-mode)
  :config
  (define-typo-cycle
      typo-cycle-dashes
    "Cycle through various dashes."
    ("-"        ; HYPHEN-MINUS
     "–"        ; EN DASH
     "—"        ; EM DASH
     "−"        ; MINUS SIGN
     "‐"        ; HYPHEN
     "‑"        ; NON-BREAKING HYPHEN
     "⸺"        ; TWO-EM DASH (added)
     "―"        ; HORIZONTAL BAR (added)
     )))

(use-package whole-line-or-region
  ;; Operate on current line if region undefined.
  )

;;; UNDO/REDO

(use-package point-undo
  :bind ( ("M-[" . point-undo)
          ("M-]" . point-redo) ))

(use-package undo-fu-session
  :hook (on-first-input . undo-fu-session-global-mode))

(use-package vundo
  :bind ( ("C-c C-/" . vundo)
          ("C-/" . undo)
          ("C-?" . undo-redo) ))

;;; SPELLING

(use-package flyspell
  :straight nil
  :bind ( :map flyspell-mode-map
          ("C-;" . flyspell-auto-correct-previous-word) )  ; or `M-s M-s'?
  :hook ((prog-mode
          shell-script-mode
          text-mode) . flyspell-prog-mode)
  :config
  ;; NOTE(2024-09-06): The following override advice is to "fix"
  ;; apparently issue with the function. It seems that the algorithm
  ;; to detect the closest misspelled word is wrong, as noted within
  ;; this function.
  (defun flyspell-ok-auto-correct-previous-word (position)
    "Auto correct the first misspelled word that occurs before point.
But don't look beyond what's visible on the screen."
    (interactive "d")

    (let ((top (window-start))
	        (bot (window-end)))
      (save-excursion
        (save-restriction
	        (narrow-to-region top bot)
	        (overlay-recenter (point))

	        (add-hook 'pre-command-hook
		                (function flyspell-auto-correct-previous-hook) t t)

	        (unless flyspell-auto-correct-previous-pos
	          ;; only reset if a new overlay exists
	          (setq flyspell-auto-correct-previous-pos nil)

	          (let ((overlay-list (seq-sort-by
                                 #'overlay-start #'< ; NOTE: the original uses #'>
                                 (overlays-in (point-min) position)))
		              (new-overlay 'dummy-value))

	            ;; search for previous (new) flyspell overlay
	            (while (and overlay-list ; NOTE: the original tests `new-overlay'
			                    (or (not (flyspell-overlay-p new-overlay))
			                        ;; check if its face has changed
			                        (not (eq (get-char-property
				                                (overlay-start new-overlay) 'face)
				                               'flyspell-incorrect))))
	              (setq new-overlay (car-safe overlay-list))
	              (setq overlay-list (cdr-safe overlay-list)))

	            ;; if nothing new exits new-overlay should be nil
	            (if new-overlay ;; the length of the word may change so go to the start
		              (setq flyspell-auto-correct-previous-pos
		                    (overlay-start new-overlay)))))

	        (when flyspell-auto-correct-previous-pos
	          (save-excursion
	            (goto-char flyspell-auto-correct-previous-pos)
	            (let ((ispell-following-word t)) ;; point is at start
	              (if (numberp flyspell-auto-correct-previous-pos)
		                (goto-char flyspell-auto-correct-previous-pos))
	              (flyspell-auto-correct-word))
	            ;; the point may have moved so reset this
	            (setq flyspell-auto-correct-previous-pos (point))))))))

  (advice-add #'flyspell-auto-correct-previous-word
              :override #'flyspell-ok-auto-correct-previous-word))

(use-package ispell
  :straight nil
  :custom ((ispell-dictionary "en_US")
           (ispell-local-dictionary-alist
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t
               ("-d" "en_US") nil utf-8)
              ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t
               ("-d" "en_GB") nil utf-8)))
           (ispell-program-name "/usr/bin/aspell")))

;;; MACRO AND EXPANSIONS

(use-package hippie-exp
  ;; Expand the word before the point in various ways.
  :straight nil
  :bind ([remap dabbrev-expand] . hippie-expand))

(provide 'subsys-editing)
;;; subsys-editing.el ends here
