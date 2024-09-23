;;; anki.el --- Anki  -*- lexical-binding: t -*-
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
  :straight (:host github :repo "anki-editor/anki-editor"
                   :fork (:host github :repo "okomestudio/anki-editor"
                                :branch "field-swap"))
  :commands (anki-editor-push-note-at-point)
  :ensure-system-package (curl . "sudo apt install -y curl")
  :custom ((anki-editor-latex-style 'mathjax)
           (anki-editor-org-tags-as-anki-tags t)
           ;; (request-log-level 'debug)  ; for debugging
           )
  :preface
  (put 'anki-editor-ignored-org-tags 'safe-local-variable #'listp))

;;; anki.el ends here
