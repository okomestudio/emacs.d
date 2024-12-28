;;; subsys-doc-viewer.el --- Document Viewer Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the document viewer subsystem.
;;
;;; Code:

;;; ePub

(use-package nov
  ;; To activate this mode, open the ePub doc and `M-x nov-mode'.
  )

;;; PDF

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda() (nlinum-mode -1)))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq pdf-view-resize-factor 1.1))

(provide 'subsys-doc-viewer)
;;; subsys-doc-viewer.el ends here
