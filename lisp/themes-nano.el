;;; themes-nano.el --- Nano Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/rougier/nano-theme
;;
;;; Code:

(use-package nano-theme
  :straight (nano-theme :host github :repo "rougier/nano-theme")
  :custom (nano-window-divider-show t))

(load-theme 'nano-dark t t)
(load-theme 'nano-light t t)

(provide 'themes-nano)
;;; themes-nano.el ends here
