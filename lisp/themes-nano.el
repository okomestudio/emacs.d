;;; themes-nano.el --- Nano Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nano-theme
  :straight (:host github :repo "rougier/nano-theme")
  :custom (nano-window-divider-show t))

(setopt themes-default 'nano-light)

(provide 'themes-nano)
;;; themes-nano.el ends here
