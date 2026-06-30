;;; pkg-anki-editor.el --- Anki Editor  -*- lexical-binding: t -*-
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

;; NOTE(2026-06-28): The last working version actually in use was:
;;
;;   ("anki-editor" . "4f5606cebe5c13b086d9e9166c15cbf99a6fd4a9")
;;
;; from the branch 'enable-file-based-note'. This is a commit on 2024-10-19. See
;; if syncing the master branch with the upstream won't cause any issues. The
;; much of context in which TS was working is now lost, but resume work on that
;; branch if necessary after merging the current master.

(use-package anki-editor
  :custom ((anki-editor-latex-style 'mathjax)
           (anki-editor-org-tags-as-anki-tags t)
           (anki-editor-swap-two-fields '("Cloze"))
           ;; (request-log-level 'debug)  ; for debugging
           )
  :commands (anki-editor-push-note-at-point)
  :ensure-system-package (curl . "sudo apt install -y curl")
  :config (require 'anki-editor-cloze-render-mode)
  :hook (org-mode . anki-editor-cloze-render-mode))

(provide 'pkg-anki-editor)
;;; pkg-anki-editor.el ends here
