;;; init-vterm.el --- vterm  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  ;; Emacs libvterm integration.

  :bind
  (:map vterm-mode-map
        ("C-c C-t" . vterm-copy-mode))

  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "vterm %s")
  (vterm-install t)
  (vterm-max-scrollback 5000)
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-timer-delay 0.02)

  :ensure-system-package
  (("/usr/include/vterm.h" . "sudo apt install -y libvterm-dev")
   ("/usr/bin/cmake" . "sudo apt install -y cmake")
   ("/usr/bin/libtool" . "sudo apt install -y libtool-bin"))

  :hook
  ((vterm-mode . (lambda ()
                   (ts/vterm-configure-faces)
                   (setq-local global-hl-line-mode nil
                               solaire-mode nil))))

  :init
  (defun ts/vterm-configure-faces ()
    (face-remap-add-relative 'default :background "#ffffdd")
    ;; (set-face-attribute 'vterm-color-black nil :foreground "#000000" :background "#000000")
    (set (make-local-variable 'buffer-face-mode-face) '(:family "Hack" :foreground "#000000"))
    (buffer-face-mode t)))

(use-package multi-vterm
  ;; Managing multiple vterm buffers.
  :after vterm)

(provide 'init-vterm)
;;; init-vterm.el ends here
