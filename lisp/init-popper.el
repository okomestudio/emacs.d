;;; init-popper.el --- Popper  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs minor-mode to summon and dismiss buffers easily.
;;
;;; Code:


(use-package popper
  :bind (("C-~"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))

  :custom
  (popper-reference-buffers
   '(
     "\\*Async Shell Command\\*"
     "\\*Async-native-compile-log\\*"
     "\\*Buffer List\\*"
     "\\*direnv\\*$"
     "\\*elfeed-log\\*$"
     "\\*lsp-log\\*$"
     "\\*Messages\\*"
     "\\*scratch\\*$"
     "\\*straight-process\\*$"
     ;; "\\*Warnings\\*"
     ;; "CAPTURE-.*\\.org$"
     "Output\\*$"
     compilation-mode
     ;;"elfeed-entry" elfeed-show-mode elfeed-goodies-show-mode
     help-mode
     helpful-mode
     magit-mode "magit:"
     ))
  (popper-window-height (lambda (win)
                          (fit-window-to-buffer
                           win
                           (* 2 (floor (frame-height) 5))
                           (floor (frame-height) 3))))

  :init
  (popper-mode +1)
  (popper-echo-mode +1))


(provide 'init-popper)
;;; init-popper.el ends here
