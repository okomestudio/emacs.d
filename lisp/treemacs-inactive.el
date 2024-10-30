;;; treemacs-inactive.el --- Treemacs (inactive)  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Treemacs configuration.
;;
;;; Code:

(use-package treemacs-nerd-icons
  ;; NOTE(2023-12-29): Disabled since char alignment is poor
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-all-the-icons
  ;; NOTE(2023-05-11): Disabled due to formatting issue
  :after (all-the-icons)
  :custom (all-the-icons-scale-factor 1.0)
  :config
  (treemacs-load-theme "all-the-icons")
  (treemacs-resize-icons 20))

;;; treemacs-inactive.el ends here
