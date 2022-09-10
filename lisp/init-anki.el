;;; init-anki.el --- Anki  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; anki-editor - Emacs minor mode for making Anki cards with Org
;; https://github.com/louietan/anki-editor
(use-package anki-editor
  :custom
  (anki-editor-use-math-jax t)
  (request-log-level 'debug)
  :ensure-system-package
  (curl . "sudo apt install curl"))

(provide 'init-anki)
;;; init-anki.el ends here
