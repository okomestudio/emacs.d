;;; 99-optimizations.el --- Optimizations  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :straight nil

  :custom
  (byte-compile-warnings '(cl-functions))
  (jit-lock-defer-time 0.05)
  (package-native-compile t)

  (read-process-output-max (* 4 1024 1024)) ; 4 mb
  (process-adaptive-read-buffering nil)

  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (inhibit-compacting-font-caches t)

  (idle-update-delay 1.0)

  :hook
  (minibuffer-setup . (lambda ()
                        (setopt gc-cons-threshold most-positive-fixnum)))
  (minibuffer-exit . (lambda ()
                       (setopt gc-cons-threshold (* 32 1024 1024))))

  :init
  ;; Suppress bidirectional parentheses algorithm for LTR only sessions:
  (setq-default bidi-display-reordering t
                bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t))


(use-package gcmh
  ;; The Garbage Collector Magic Hack.
  :defer nil

  :hook
  (after-init . gcmh-mode)
  (focus-out-hook . garbage-collect)

  :custom
  (gcmh-high-cons-threshold (* 32 1024 1024))
  (gcmh-idle-delay 5))

;;; 99-optimizations.el ends here
