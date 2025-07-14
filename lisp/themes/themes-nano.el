;;; themes-nano.el --- Nano Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/rougier/nano-theme
;;
;;; Code:

(use-package nano-theme
  :straight (nano-theme
             :type git :host github :repo "rougier/nano-theme"
             :commit "ffe414c8af9c673caf8b8b05ba89a229cb9ad48b")
  :custom (nano-window-divider-show t)
  :config
  (load-theme 'nano-dark t t)
  (load-theme 'nano-light t t))

(provide 'themes-nano)
;;; themes-nano.el ends here
