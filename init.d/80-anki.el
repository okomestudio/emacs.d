;;; 80-anki.el --- anki  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Anki and related utilities.
;;
;;; Code:

(use-package anki-editor
  ;; Emacs minor mode for making Anki cards with Org.
  :straight (:fork "orgtre")
  :commands (anki-editor-note-at-point)
  :ensure-system-package (curl . "sudo apt install -y curl")

  :custom
  ;; (request-log-level 'debug) ;; for debugging
  (anki-editor-latex-style 'mathjax)
  (anki-editor-ignored-org-tags '("export" "noexport" "anki"))
  (anki-editor-org-tags-as-anki-tags t)

  :init
  (with-eval-after-load 'org
    (add-to-list 'org-global-properties-fixed '("ANKI_DECK_ALL" . "Default"))
    (add-to-list 'org-global-properties-fixed
                 `("ANKI_NOTE_TYPE_ALL"
                   . ,(string-join '("Basic"
                                     "\"Basic (and reversed card)\""
                                     "Cloze") " ")))
    (add-to-list 'org-default-properties "ANKI_DECK")
    (add-to-list 'org-default-properties "ANKI_NOTE_ID")
    (add-to-list 'org-default-properties "ANKI_NOTE_TYPE")))

;;; 80-anki.el ends here
