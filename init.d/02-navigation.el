;;; 02-navigation.el --- Navigation  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WINDOWS AND FRAMES

(use-package ace-window
  :defer t

  :bind
  ("M-O" . 'ace-window)

  :custom
  (aw-dispatch-always t))


(use-package frame-cmds
  :defer t

  :bind
  ("M-o" . 'other-window-or-frame))


(use-package winner
  ;; Undo or redo a change in window configuration.
  :defer t
  :straight nil

  :bind
  ("C-c <right>" . winner-redo)
  ("C-c <left>". winner-undo)

  :hook
  (after-init . (lambda () (winner-mode 1))))


;; IMENU

(use-package imenu-list :defer t)

;;; 02-navigation.el ends here
