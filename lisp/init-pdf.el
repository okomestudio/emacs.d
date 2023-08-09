;;; init-pdf.el --- PDF  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package pdf-tools
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda() (nlinum-mode -1)))
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq pdf-view-resize-factor 1.1))


(provide 'init-pdf)
;;; init-pdf.el ends here
