;;; themes-kanagawa.el --- Kanagawa Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/Fabiokleis/kanagawa-emacs
;;
;;; Code:

(use-package kanagawa-themes
  :straight (kanagawa-themes :host github
                             :repor "Fabiokleis/kanagawa-emacs"
                             :commit "1d34a95c0f639b035481b5506089bc209769bab6")
  :config
  (load-theme 'kanagawa-dragon t t)
  (load-theme 'kanagawa-lotus t t)
  (load-theme 'kanagawa-wave t t))

(provide 'themes-kanagawa)
;;; themes-kanagawa.el ends here
