;;; 20-vterm.el --- vterm  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure vterm related utilities.
;;
;;; Code:

(use-package vterm
  ;; Emacs libvterm integration.
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "vterm %s")
  (vterm-max-scrollback 5000)
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-timer-delay 0.02)

  ;; Need to unset this to let the shell's pyenv manage venvs:
  (vterm-environment '("PYENV_VERSION="))

  :ensure-system-package
  ("/usr/include/vterm.h" . "sudo apt install -y libvterm-dev")
  ("/usr/bin/cmake" . "sudo apt install -y cmake")
  ("/usr/bin/libtool" . "sudo apt install -y libtool-bin")

  :hook
  (vterm-mode . (lambda ()
                  (setq-local global-hl-line-mode nil
                              solaire-mode nil
                              buffer-face-mode-face 'fixed-pitch)
                  (buffer-face-mode t))))


(use-package multi-vterm
  :after vterm)

;;; 20-vterm.el ends here
