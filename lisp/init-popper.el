;;; init-popper.el --- GPT clients  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package popper
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
    "Output\\*$"
    "\\*Async Shell Command\\*"
    "elfeed-entry" elfeed-show-mode
    "magit:" magit-mode
    helpful-mode
    help-mode
    compilation-mode))

  :bind (("C-~"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))

  :init
  (popper-mode +1)
  (popper-echo-mode +1))


(provide 'init-popper)
;;; init-popper.el ends here
