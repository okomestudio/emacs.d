;;; themes-doom.el --- Doom Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/doomemacs/themes
;;
;;; Code:

(require 'ok)

(use-package doom-themes
  :straight (doom-themes
             :type git :host github :repo "doomemacs/themes"
             :commit "88126db5e63d816533d0372cb99246b842cac74e")
  :config
  (load-theme 'doom-one t t)
  (load-theme 'doom-one-light t t)
  (load-theme 'doom-opera t t)
  (load-theme 'doom-opera-light t t))

(provide 'doom-nano)
;;; themes-doom.el ends here
