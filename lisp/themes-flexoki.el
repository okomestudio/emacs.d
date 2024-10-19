;;; themes-flexoki.el --- Flexoki Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flexoki-themes
  :hook
  (after-load-theme
   . (lambda ()
       ;; Recover mode line box border.
       (let ((box-color (face-attribute 'mode-line :background)))
         (set-face-attribute 'mode-line nil :box box-color)
         (set-face-attribute 'mode-line-active nil :box box-color)
         (set-face-attribute 'mode-line-inactive nil :box box-color))))

  :config
  (with-eval-after-load 'corfu
    (let ((mode (frame-parameter nil 'background-mode)))
      (when (string= mode "light")
        (let ((bg "#f0f0f0"))
          (set-face-attribute 'corfu-current nil :background bg)
          (set-face-attribute 'corfu-default nil :background bg)
          (with-eval-after-load 'corfu-popupinfo
            (set-face-attribute 'corfu-popupinfo nil :background bg))))))

  (with-eval-after-load 'company
    (let ((mode (frame-parameter nil 'background-mode)))
      (when (string= mode "light")
        (set-face-attribute 'company-tooltip nil :background "#eeeeee")))))

;;; themes-flexoki.el ends here