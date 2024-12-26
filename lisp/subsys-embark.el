;;; subsys-embark.el --- Embark Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Embark subsystem.
;;
;;; Code:

(use-package embark
  ;; Emacs Mini-Buffer Actions Rooted in Key maps.
  :bind (("C-." . embark-act)
         :map help-map
         ("B" . embark-bindings)))

(use-package embark-consult
  ;; Use if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer.
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'subsys-embark)
;;; subsys-embark.el ends here
