;;; themes-spacemacs.el --- Spacemacs Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ok)

(use-package spacemacs-theme
  :hook (before-enable-theme-functions . spacemacs-theme-ok--prepare)
  :config
  (defun spacemacs-theme-ok--prepare (theme)
    (when (member theme '(spacemacs-dark spacemacs-light))
      (setopt window-divider-default-bottom-width 1
              window-divider-default-right-width 1)
      (pcase theme
        ('spacemacs-dark
         (setq spacemacs-theme-custom-colors nil))
        ('spacemacs-light
         (setq spacemacs-theme-custom-colors
               `((base . "#322938") ;; #655370 for light, true-color

                 ;; Make some colors slightly darker
                 (head3 . ,(ok-face-color-scale "#67b11d" 0.80))
                 (head4 . ,(ok-face-color-scale "#b1951d" 0.80))
                 (cyan . ,(ok-face-color-scale "#21b8c7" 0.95)))))))))

(load-theme 'spacemacs-dark t t)
(load-theme 'spacemacs-light t t)

(provide 'themes-spacemacs)
;;; themes-spacemacs.el ends here
