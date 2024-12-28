;;; subsys-drawing.el --- Drawing Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the drawing subsystem.
;;
;;; Code:

(use-package org-excalidraw
  ;; Excalidraw in Org.
  ;;
  ;; TODO: Enable SVG rendering. The current version Excalidraw does not
  ;; render texts properly. Wait till the fix in PR
  ;; https://github.com/excalidraw/excalidraw/pull/8016 gets merged and
  ;; released to see if that resolves the issue.
  :straight (org-excalidraw :host github
                            :repo "4honor/org-excalidraw"
                            :fork (:host github
                                         :repo "okomestudio/org-excalidraw"
                                         :branch "custom-exec"))
  :custom ((kroki-exec "~/.config/emacs/bin/kroki"))
  :commands (org-excalidraw-create-drawing)
  :hook (on-first-input . (lambda () (require 'org-excalidraw)))
  :ensure-system-package
  (kroki . "go install github.com/yuzutech/kroki-cli/cmd/kroki@latest"))

(provide 'subsys-drawing)
;;; subsys-drawing.el ends here
