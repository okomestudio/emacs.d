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
  :straight (:host github :repo "anki-editor/anki-editor")
  :commands (anki-editor-push-note-at-point)
  :ensure-system-package (curl . "sudo apt install -y curl")
  :custom ((anki-editor-latex-style 'mathjax)

           ;; Use Org tags for Anki, but specify those to be ignored
           (anki-editor-org-tags-as-anki-tags t)
           (anki-editor-ignored-org-tags '("anki"
                                           "export"
                                           "m"
                                           "main"
                                           "noexport"
                                           "z"))
           ;; (request-log-level 'debug)  ; for debugging
           )
  :init
  (with-eval-after-load 'org
    (let ((props '(("ANKI_DECK" . ("Default"))
                   ("ANKI_NOTE_ID")
                   ("ANKI_NOTE_TYPE" . ("Basic"
                                        "Basic (and reversed card)"
                                        "Cloze")))))
      (dolist (prop props)
        (let ((keyword (car prop))
              (values (cdr prop)))
          (when values
            (add-to-list 'org-global-properties-fixed
                         `(,(concat keyword "_ALL")
                           .
                           ,(mapconcat (lambda (s)
                                         (format "\"%s\"" s))
                                       values
                                       " "))))
          (add-to-list 'org-default-properties keyword))))))

;;; anki.el ends here
