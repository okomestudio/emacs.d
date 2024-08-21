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

(defconst ok-debug nil
  "Global flag to activate all debug-related flag.")

(setopt debug-on-error ok-debug)
(setq debug-on-message nil) ;; set regexp to trigger debugger
(setopt byte-compile-warnings '(not obsolete)) ;; set t for development
(setopt package-enable-at-startup nil)
(setopt inhibit-default-init nil)

;; Reduce GC usage while initialization. 800 kb is the default (2021-08-01).
;; Note that the threshold while running is set by gcmh later in init and the
;; following temporary setting will be overridden. Use that for adjustment.
(setopt gc-cons-threshold most-positive-fixnum)

;; UI:
(setopt frame-inhibit-implied-resize t)
(setopt inhibit-splash-screen nil)
(setopt inhibit-startup-screen nil)
(setopt initial-buffer-choice nil)
(setopt native-comp-async-report-warnings-errors ok-debug)
(setq native-comp-jit-compilation t)
(setq redisplay-skip-fontification-on-input t)
(setopt ring-bell-function 'ignore) ;; disable beeping (in C source code)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(fringe-mode '(12 . 12))

;; Adjust default frame size if desired:
;; (add-to-list 'default-frame-alist (cons 'width 120))
;; (add-to-list 'default-frame-alist (cons 'height 40))

;; Misc. configurations
(setenv "LSP_USE_PLISTS" "true")

;; Ignore x session resources:
(advice-add 'x-apply-session-resources :override 'ignore)

;;; early-init.el ends here
