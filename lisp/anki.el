;;; anki.el --- anki  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Anki (apps.ankiweb.net) is a tool for spaced repetition.
;;
;; The `anki-editor' minor mode enables editing of Anki notes in Org
;; mode. Sync is unidirectional, i.e., the notes can only be pushed
;; from Org, but not the other way.
;;
;; To use the `anki-editor' mode, the AnkiConnect add-on needs to be
;; installed on the Anki app client (Tools -> Add-ons).
;;
;;; Code:

(use-package anki-editor
  ;; Emacs minor mode for making Anki cards with Org.
  :straight (:repo "anki-editor/anki-editor")
  :commands (anki-editor-note-at-point)
  :ensure-system-package (curl . "sudo apt install -y curl")

  :custom
  (anki-editor-latex-style 'mathjax)

  ;; Use Org tags for Anki, but specify those to be ignored:
  (anki-editor-org-tags-as-anki-tags t)
  (anki-editor-ignored-org-tags '("anki"
                                  "export"
                                  "m"
                                  "main"
                                  "noexport"
                                  "z"))

  ;; (request-log-level 'debug) ;; for debugging

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

;;; anki.el ends here
