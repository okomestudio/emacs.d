;;; themes-year-1984.el --- Year 1984 Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/rougier/nano-theme
;;
;;; Code:

(use-package year-1984-theme
  :straight (year-1984-theme :type git
                             :host github
                             :repo "mastro35/year-1984-theme")
  :demand t
  ;; :config
  ;; NOTE(2025-08-13): The theme code adds itself via autoload, so `load-theme'
  ;; here will recurse infinitely.
  ;; (load-theme 'year-1984 t t)
  )

(provide 'themes-year-1984)
;;; themes-year-1984.el ends here
