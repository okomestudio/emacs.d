;;; themes-flexoki.el --- Flexoki Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; See https://github.com/crmsnbleyd/flexoki-emacs-theme.
;;
;;; Code:

(use-package flexoki-themes
  :hook (after-load-theme . flexoki-themes-ok--modeline-box-border)
  :config
  (defun flexoki-themes-ok--modeline-box-border ()
    "Set the mode line box border color."
    (let ((box-color (face-attribute 'mode-line :background)))
      (set-face-attribute 'mode-line nil :box box-color)
      (set-face-attribute 'mode-line-active nil :box box-color)
      (set-face-attribute 'mode-line-inactive nil :box box-color)))

  ;; (with-eval-after-load 'corfu
  ;;   (let ((mode (frame-parameter nil 'background-mode)))
  ;;     (when (string= mode "light")
  ;;       (let ((bg "#f0f0f0"))
  ;;         (set-face-attribute 'corfu-current nil :background bg)
  ;;         (set-face-attribute 'corfu-default nil :background bg)
  ;;         (with-eval-after-load 'corfu-popupinfo
  ;;           (set-face-attribute 'corfu-popupinfo nil :background bg))))))

  ;; (with-eval-after-load 'company
  ;;   (let ((mode (frame-parameter nil 'background-mode)))
  ;;     (when (string= mode "light")
  ;;       (set-face-attribute 'company-tooltip nil :background "#eeeeee"))))

  (with-eval-after-load 'vertico
    (let ((background (face-attribute 'default :background)))
      (set-face-attribute 'vertico-current nil :background background))))

(setopt themes-default 'flexoki-themes-light
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)

(provide 'themes-flexoki)
;;; themes-flexoki.el ends here
