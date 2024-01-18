;;; init-navigation.el --- Navigation  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; WINDOWS AND FRAMES

(use-package ace-window
  ;; Quickly switch windows in Emacs.
  :bind
  ("M-o" . 'other-window-or-frame)
  ("M-O" . 'ace-window)

  :custom
  (aw-dispatch-always t))


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
