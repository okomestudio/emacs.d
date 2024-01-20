;;; init-optimizations.el --- Optimizations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package bytecomp
  :straight nil

  :custom
  (byte-compile-warnigns '(cl-functions)))


(use-package gcmh
  ;; The Garbage Collector Magic Hack.
  :defer nil

  :hook
  (after-init . gcmh-mode)
  (focus-out-hook . garbage-collect)

  :custom
  (gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-idle-delay 5))


(provide 'init-optimizations)
;;; init-optimizations.el ends here
