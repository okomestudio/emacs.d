;;; init-tetris.el --- Tetris  -*- lexical-binding: t -*-
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


(provide 'init-tetris)
;;; init-tetris.el ends here
