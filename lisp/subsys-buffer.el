;;; subsys-buffer.el --- Buffer Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the buffer subsystem.
;;
;;; Code:

(use-package popper
  ;; A minor-mode to summon and dismiss buffers easily.
  :bind (("C-'" . popper-toggle)
         ("C-S-'" . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :custom ((popper-reference-buffers
            '("\\*Async Shell Command\\*\\'"
              "\\*Async-native-compile-log\\*\\'"
              "\\*Backtrace\\*\\'"
              "\\*Buffer List\\*\\'"
              "\\*ChatGPT>"
              "\\*devdocs\\*\\'"
              "\\*direnv\\*\\'"
              "\\*elfeed-log\\*\\'"
              "\\*init log\\*\\'"
              "\\*lsp-log\\*\\'"
              "\\*Messages\\*\\'"
              "\\*scratch\\*\\'"
              "\\*straight-process\\*\\'"
              "\\*Warnings\\*\\'"
              ;; "CAPTURE-.*\\.org\\'"
              "Output\\*\\'"
              comint-mode
              compilation-mode
              help-mode
              helpful-mode
              magit-process-mode
              magit-status-mode))
           (popper-display-control t)
           (popper-display-function #'display-buffer-pop-up-window)
           (popper-window-height (lambda (win)
                                   (fit-window-to-buffer
                                    win
                                    (* 2 (floor (frame-height) 5))
                                    (floor (frame-height) 3)))))
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(provide 'subsys-buffer)
;;; subsys-buffer.el ends here
