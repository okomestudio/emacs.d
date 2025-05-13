;;; early-init.el --- Emacs early init configuration  -*- lexical-binding: t -*-
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/emacs.d
;; Keywords: config
;; Package-Requires: ((emacs "30.1"))
;;
;;; Commentary:
;;
;; Provides early initialization for Emacs.
;;
;;; Code:

;;; Init to do at the earliest
;; Some configuration must be done at the earliest points in initialization.

(startup-redirect-eln-cache (convert-standard-filename
                             (locate-user-emacs-file "var/eln-cache/")))

;;; Debug switches

(setopt ok-debug nil)         ; global debug switch
(setopt debug-on-error ok-debug)
(setopt warning-minimum-level :warning
        warning-suppress-types nil)
(setq debug-on-message nil)   ; set regexp to trigger debugger
(setq byte-compile-warnings '(not obsolete)) ; set t for development

;;; Package manager

(setopt package-enable-at-startup nil)
(load (locate-user-emacs-file "lisp/init-straight.el")) ; or `init-package.el'

;;; GC and native compilation

;; Reduce GC while initialization by increasing the threshold (800 kb
;; is the default on circa 2021-08-01). Note that the threshold
;; reverts to another value after initialization; see
;; `optimizations.el'.
(setopt gc-cons-threshold most-positive-fixnum)

(setq native-comp-jit-compilation t)

;;; UX/UI

(setopt confirm-kill-processes t
        frame-inhibit-implied-resize t
        inhibit-default-init nil
        inhibit-startup-screen t
        initial-buffer-choice nil
        native-comp-async-query-on-exit t
        native-comp-async-report-warnings-errors ok-debug
        pgtk-wait-for-event-timeout 0
        ring-bell-function 'ignore)
(setq redisplay-skip-fontification-on-input t)

(dolist (it '((menu-bar-lines . 0)
              (tool-bar-lines . 0)
              ;; (width . 120)  ; default frame width
              ;; (height . 40)  ; default frame height
              (vertical-scroll-bars . nil)))
  (push it default-frame-alist))

(fringe-mode '(12 . 12))

;; Ignore x session resources.
(advice-add 'x-apply-session-resources :override 'ignore)

;;; Environment variables

(setenv "LSP_USE_PLISTS" "true")

;;; Misc. optimizations

;; Disable magic file name during `init.el'.
(letrec ((saved-file-name-handler-alist file-name-handler-alist)
         (restore-file-name-handler-alist
          (lambda ()
            "Restore `file-name-handler-alist'."
            (setq file-name-handler-alist saved-file-name-handler-alist)
            (remove-hook 'after-init-hook restore-file-name-handler-alist))))
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook restore-file-name-handler-alist 97))

;; Profile `init.el'.
(when ok-debug
  (letrec ((profile-init
            (lambda ()
              (message "Emacs (PID:%d) started in %s"
                       (emacs-pid) (emacs-init-time))
              (profiler-report)
              (profiler-stop)
              (remove-hook 'after-init-hook profile-init))))
    (profiler-start 'cpu+mem)
    (add-hook 'after-init-hook profile-init 98)))

;; Compute the running time of all functions in `after-init-hook'.
;; `emacs-init-time' underestimates the total startup time, when
;; time-consuming operations are delayed to `after-init-hook'. Use
;; this metric to actually reduce the experienced startup time.
(letrec ((time-after-init-hook
          (lambda ()
            (message "after-init-hook took %f sec"
                     (float-time
                      (time-subtract (current-time) after-init-time)))
            (remove-hook 'after-init-hook time-after-init-hook))))
  (add-hook 'after-init-hook time-after-init-hook 99))

(provide 'early-init)
;;; early-init.el ends here
