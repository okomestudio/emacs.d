;;; early-init.el --- early-init  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;
;; Provides early initialization for Emacs.
;;
;;; Code:

;; Redirect native compilation cache if possible.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "The minimum Emacs version expected is %s" minver)))

(setq ok-debug nil) ;; global flag to manage all debug-related flags

(setq debug-on-error ok-debug)
(setq byte-compile-warnings '(not obsolete)) ;; set t for development
(setq package-enable-at-startup nil)

;; Reduce GC usage while initialization. 800 kb is the default (2021-08-01).
;; Note that the threshold while running is set by gcmh later in init and the
;; following temporary setting will be overridden. Use that for adjustment.
(setq gc-cons-threshold most-positive-fixnum)

(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen nil)
(setq inhibit-startup-screen nil)
(setq initial-buffer-choice nil)
(setq native-comp-jit-compilation t)
(setq redisplay-skip-fontification-on-input t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(fringe-mode '(12 . 12))

;; Ignore x session resources
(advice-add 'x-apply-session-resources :override 'ignore)

;;; early-init.el ends here
