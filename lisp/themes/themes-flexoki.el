;;; themes-flexoki.el --- Flexoki Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/crmsnbleyd/flexoki-emacs-theme
;;
;;; Code:

(require 'ok)

(use-package flexoki-themes
  :straight (flexoki-themes :host github
                            :repo "crmsnbleyd/flexoki-emacs-theme"
                            :commit "4ca5d80bc4f33b5ace8950f0c00069539835fab4")
  :hook ((enable-theme-functions . flexoki-themes-ok--prepare)
         (enable-theme-functions . flexoki-themes-ok--modeline-box-border))
  :config
  (defun flexoki-themes-ok--modeline-box-border (theme)
    "Set the mode line box border color."
    (when (or (eq theme 'flexoki-themes-dark) (eq theme 'flexoki-themes-light))
      (let ((box-color (face-attribute 'mode-line :background)))
        (set-face-attribute 'mode-line nil :box box-color)
        (set-face-attribute 'mode-line-active nil :box box-color)
        (set-face-attribute 'mode-line-inactive nil :box box-color))))

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

  (defun flexoki-themes-ok--prepare (theme)
    (when (or (eq theme 'flexoki-themes-dark) (eq theme 'flexoki-themes-light))
      (with-eval-after-load 'vertico
        (let ((background (face-attribute 'default :background)))
          (set-face-attribute 'vertico-current nil :background background)))

      (setopt window-divider-default-bottom-width 1
              window-divider-default-right-width 1)))

  (load-theme 'flexoki-themes-dark t t)
  (load-theme 'flexoki-themes-light t t))

(provide 'themes-flexoki)
;;; themes-flexoki.el ends here
