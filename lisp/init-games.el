;;; init-games.el --- Games  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package tetris
  :custom
  (tetris-use-color nil)
  (tetris-x-colors [[0.5 0.5 1]
                    [0.7 0 1]
                    [1 1 0]
                    [1 0 1]
                    [0 1 1]
                    [0 1 0]
                    [1 0 0]]))


(provide 'init-games)
;;; init-games.el ends here