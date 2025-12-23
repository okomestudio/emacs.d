;;; subsys-spelling.el --- Spelling Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the spelling subsystem.
;;
;;; Code:

(use-package flyspell
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

  (advice-add #'flyspell-auto-correct-previous-word :override
              #'flyspell-ok-auto-correct-previous-word))

(use-package ispell
  :custom ((ispell-dictionary "en_US")
           (ispell-local-dictionary-alist
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t
               ("-d" "en_US") nil utf-8)
              ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t
               ("-d" "en_GB") nil utf-8)))
           (ispell-program-name "/usr/bin/aspell")))

;; Jinx (github.com/minad/jinx): A fast JIT spell checker using Enchant. Could
;; use this in place of ispell/aspell (2025-12-22).

(provide 'subsys-spelling)
;;; subsys-spelling.el ends here
