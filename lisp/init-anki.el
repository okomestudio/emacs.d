;;; init-anki.el --- Anki  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package anki-editor
  ;; Emacs minor mode for making Anki cards with Org.
  :defer t
  :straight (:fork "orgtre")
  :ensure-system-package (curl . "sudo apt install -y curl")

  :custom
  (anki-editor-latex-style 'mathjax)
  ;; (request-log-level 'debug)
  )

(provide 'init-anki)
;;; init-anki.el ends here
