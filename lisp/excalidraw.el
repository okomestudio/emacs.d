;;; excalidraw.el --- Excalidraw  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Excalidraw configuration.
;;
;;; Code:

(use-package org-excalidraw
  :straight (:host github :repo "4honor/org-excalidraw")
  :commands (org-excalidraw-create-drawing)
  :hook (on-first-input . (lambda () (require 'org-excalidraw)))
  :ensure-system-package
  (kroki . "go install github.com/yuzutech/kroki-cli/cmd/kroki@latest"))

;;; excalidraw.el ends here
