;;; themes-spacemacs.el --- Spacemacs Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package spacemacs-theme
  :config
  (let ((mode (frame-parameter nil 'background-mode)))
    (if (string= mode "light")
        (setq spacemacs-theme-custom-colors
              `((base . "#322938") ;; #655370 for light, true-color

                ;; Make some colors slightly darker
                (head3 . ,(ok-face-color-scale "#67b11d" 0.80))
                (head4 . ,(ok-face-color-scale "#b1951d" 0.80))
                (cyan . ,(ok-face-color-scale "#21b8c7" 0.95)))))))

;;; themes-spacemacs.el ends here