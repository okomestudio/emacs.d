;;; init-windows.el --- Windows  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Windows placement.
;;
;;; Code:


(use-package popper
  ;; Emacs minor-mode to summon and dismiss buffers easily.
  :bind
  (("C-~"   . popper-toggle)
   ("C-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))

  :custom
  (popper-reference-buffers
   '("\\*Async Shell Command\\*\\'"
     "\\*Async-native-compile-log\\*\\'"
     "\\*Backtrace\\*\\'"
     "\\*Buffer List\\*\\'"
     "\\*devdocs\\*\\'"
     "\\*direnv\\*\\'"
     "\\*elfeed-log\\*\\'"
     "\\*lsp-log\\*\\'"
     "\\*Messages\\*\\'"
     "\\*scratch\\*\\'"
     "\\*straight-process\\*\\'"
     "\\*Warnings\\*\\'"
     ;; "CAPTURE-.*\\.org\\'"
     "Output\\*\\'"
     compilation-mode
     help-mode
     helpful-mode
     magit-status-mode))
  (popper-display-control t)
  (popper-display-function #'display-buffer-pop-up-window)
  (popper-window-height (lambda (win)
                          (fit-window-to-buffer
                           win
                           (* 2 (floor (frame-height) 5))
                           (floor (frame-height) 3))))

  :init
  (popper-mode +1)
  (popper-echo-mode +1))


(provide 'init-windows)
;;; init-windows.el ends here
