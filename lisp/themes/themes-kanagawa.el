;;; themes-kanagawa.el --- Kanagawa Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/Fabiokleis/kanagawa-emacs
;;
;;; Code:

(require 'ok)

(use-package kanagawa-themes
  :config
  (load-theme 'kanagawa-dragon t t)
  (load-theme 'kanagawa-lotus t t)
  (load-theme 'kanagawa-wave t t))

(provide 'themes-kanagawa)
;;; themes-kanagawa.el ends here
