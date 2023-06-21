;;; early-init.el  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Provides early initialization for Emacs > 27.1.
;;;
;;; Code:

(setq debug-on-error nil)               ; set t when debugging startup issues:

(let ((minver "29.0"))
  (when (version< emacs-version minver)
    (error "The minimum Emacs version expected is %s" minver)))

;; Reduce GC usage while initialization. 800 kb is the default (2021-08-01):
(let ((init-gc-cons-threshold most-positive-fixnum))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold 800000))))

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-inhibit-implied-resize t)

(setq native-comp-jit-compilation t)

(provide 'early-init)
;;; early-init.el ends here
