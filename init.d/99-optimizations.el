;;; 99-optimizations.el --- Optimizations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :straight nil

  :custom
  (byte-compile-warnigns '(cl-functions))
  (jit-lock-defer-time 0.05)
  (package-native-compile t))


(use-package gcmh
  ;; The Garbage Collector Magic Hack.
  :defer nil

  :hook
  (after-init . gcmh-mode)
  (focus-out-hook . garbage-collect)

  :custom
  (gcmh-high-cons-threshold (* 1024 1024 1024))
  (gcmh-idle-delay 5))

;;; 99-optimizations.el ends here
