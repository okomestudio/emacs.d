;;; 80-anki.el --- Anki  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize Anki.
;;
;;; Code:

(defun anki-template ()
  "Insert template for Anki notes."
  (interactive)
  (insert "* Anki notes :anki:\n")
  (insert ":PROPERTIES:\n")
  (insert ":VISIBILITY: folded\n")
  (insert ":ANKI_DECK: Default\n")
  (insert ":END:\n")
  (org-id-get-create)
  (insert "** Note\n")
  (insert ":PROPERTIES:\n")
  (insert ":ANKI_NOTE_TYPE: Basic (and reversed card)\n")
  (insert ":ANKI_NOTE_ID:\n")
  (insert ":END:\n")
  (insert "*** Front\n")
  (insert "*** Back\n"))

(use-package anki-editor
  ;; Emacs minor mode for making Anki cards with Org.
  :straight (:fork "orgtre")

  :custom
  (anki-editor-latex-style 'mathjax)
  (anki-editor-ignored-org-tags '("export" "noexport" "anki"))
  (anki-editor-org-tags-as-anki-tags t)
  ;; (request-log-level 'debug)

  :ensure-system-package
  (curl . "sudo apt install -y curl"))

;;; 80-anki.el ends here
