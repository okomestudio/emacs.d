;;; embark.el --- Embark  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package embark
  ;; Emacs Mini-Buffer Actions Rooted in Key maps.
  :bind (("C-." . embark-act)
         :map help-map
         ("B" . embark-bindings)))

;;; embark.el ends here
