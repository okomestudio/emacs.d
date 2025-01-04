;;; optimizations.el --- Optimizations  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up optimizations for Emacs.
;;
;;; Code:

(defcustom ok-gc-high-cons-threshold (* 32 1024 1024)
  "Default high cons GC threshold.")

(use-package emacs
  :straight nil
  :custom ((byte-compile-warnings '(cl-functions))
           (jit-lock-defer-time 0.05)
           (package-native-compile t)

           (read-process-output-max (* 4 1024 1024)) ; 4 mb
           (process-adaptive-read-buffering nil)

           (fast-but-imprecise-scrolling t)
           (redisplay-skip-fontification-on-input t)
           (inhibit-compacting-font-caches t)

           (idle-update-delay 1.0))
  :hook ((minibuffer-setup . gc-cons-threshold--max-out)
         (minibuffer-exit . gc-cons-threshold--revert))
  :init
  ;; Suppress bidirectional parentheses algorithm for LTR only sessions:
  (setq-default bidi-display-reordering t
                bidi-paragraph-direction 'left-to-right
                bidi-inhibit-bpa t)

  :config
  (defun gc-cons-threshold--max-out ()
    (setopt gc-cons-threshold most-positive-fixnum))

  (defun gc-cons-threshold--revert ()
    (setopt gc-cons-threshold ok-gc-high-cons-threshold)))

(use-package gcmh
  ;; The Garbage Collector Magic Hack.
  :defer nil
  :hook ((after-init . gcmh-mode)
         (focus-out-hook . garbage-collect))
  :custom ((gcmh-high-cons-threshold ok-gc-high-cons-threshold)
           (gcmh-idle-delay 5)))

(use-package on
  ;; Hooks for faster Emacs startup.
  ;;
  ;;   - `on-first-input-hook'
  ;;   - `on-first-file-hook'
  ;;   - `on-first-buffer-hook'
  ;;   - `on-switch-buffer-hook'
  ;;   - `on-switch-window-hook'
  ;;   - `on-switch-frame-hook'
  ;;
  :demand t)

(provide 'optimization)
;;; optimizations.el ends here
