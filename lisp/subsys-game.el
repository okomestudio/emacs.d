;;; subsys-game.el --- Game Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up game subsystem.
;;
;;; Code:

(use-package tetris
  :bind (:map tetris-mode-map
              ("i" . tetris-rotate-prev)
              ("j" . tetris-move-left)
              ("k" . tetris-move-right)
              ("m" . tetris-move-down))
  :custom ((tetris-use-color nil)
           (tetris-x-colors [[0.5 0.5 1]
                             [0.7 0 1]
                             [1 1 0]
                             [1 0 1]
                             [0 1 1]
                             [0 1 0]
                             [1 0 0]])))

;; Misc.

(use-package fireplace)
(use-package snow)
(use-package xkcd)

(provide 'subsys-game)
;;; subsys-game.el ends here
