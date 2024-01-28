;;; early-init.el  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;
;; Provides early initialization for Emacs.
;;
;;; Code:

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "The minimum Emacs version expected is %s" minver)))

(setq debug-on-error t) ;; set t when debugging startup issues
(setq byte-compile-warnings '(not obsolete)) ;; set t for development

;; Reduce GC usage while initialization. 800 kb is the default (2021-08-01).
;; Note that the threshold while running is set by gcmh later in init and the
;; following temporary setting will be overridden. Use that for adjustment.
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(fringe-mode '(12 . 12))

(setq frame-inhibit-implied-resize t)

(setq native-comp-jit-compilation t)

(setq redisplay-skip-fontification-on-input t)

(setq initial-buffer-choice 'org-agenda)
(setq inhibit-splash-screen nil)
(setq inhibit-startup-screen nil)

;;; early-init.el ends here
