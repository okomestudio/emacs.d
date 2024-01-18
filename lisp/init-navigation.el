;;; init-navigation.el --- Navigation  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; WINDOWS AND FRAMES

(use-package ace-window
  :bind
  ("M-O" . 'ace-window)

  :custom
  (aw-dispatch-always t))


(use-package frame-cmds
  :bind
  ("M-o" . 'other-window-or-frame))


(use-package winner
  ;; Undo or redo a change in window configuration.
  :straight nil

  :bind
  ("C-c <right>" . winner-redo)
  ("C-c <left>". winner-undo)

  :init
  (winner-mode 1))


;; IMENU

(use-package imenu-list)


(require 'init-bookmark-plus)
(require 'init-consult)
;; (require 'init-dired)
(require 'init-hydra)
(require 'init-treemacs)
(require 'init-popper)


(provide 'init-navigation)
;;; init-navigation.el ends here
