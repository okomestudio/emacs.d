;;; init-vterm.el --- vterm  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; vterm - Emacs libvterm integration
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "vterm %s")
  (vterm-install t)

  :ensure-system-package
  (cmake . "sudo apt install -y cmake libtool-bin libvterm-dev")

  :hook
  ((vterm-mode . ts/vterm-configure-faces)
   (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
   (vterm-mode . (lambda () (setq-local solaire-mode nil))))

  :init
  (defun ts/vterm-configure-faces ()
    (face-remap-add-relative 'default :background "#ffffdd")
    ;; (set-face-attribute 'vterm-color-black nil :foreground "#000000" :background "#000000")
    ;; (set-face-attribute 'vterm-color-red nil :foreground "#000000" :background "#000000")
    ;; (set-face-attribute 'vterm-color-green nil :foreground "#000000" :background "#000000")
    ;; (set-face-attribute 'vterm-color-yellow nil :foreground "#000000" :background "#000000")
    ;; (set-face-attribute 'vterm-color-blue nil :foreground "#000000" :background "#000000")
    ;; (set-face-attribute 'vterm-color-magenta nil :foreground "#000000" :background "#000000")
    ;; (set-face-attribute 'vterm-color-cyan nil :foreground "#000000" :background "#000000")
    ;; (set-face-attribute 'vterm-color-white nil :foreground "#000000" :background "#000000")
    (set (make-local-variable 'buffer-face-mode-face) '(:family "Hack" :foreground "#000000"))
    (buffer-face-mode t))
  )

(use-package multi-vterm
  :after vterm)

(provide 'init-vterm)
;;; init-vterm.el ends here
