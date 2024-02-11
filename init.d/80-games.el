;;; 80-games.el --- Games  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize games.
;;
;;; Code:

(use-package tetris
  :bind
  (;;
   :map tetris-mode-map
   ("i" . tetris-rotate-prev)
   ("j" . tetris-move-left)
   ("k" . tetris-move-right)
   ("m" . tetris-move-down))

  :custom
  (tetris-use-color nil)
  (tetris-x-colors [[0.5 0.5 1]
                    [0.7 0 1]
                    [1 1 0]
                    [1 0 1]
                    [0 1 1]
                    [0 1 0]
                    [1 0 0]]))

;;; 80-games.el ends here
