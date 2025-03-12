;;; early-init.el --- early-init  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;
;; Provides early initialization for Emacs.
;;
;;; Code:

;; Disable `package.el' early, as we use `straight.el'.
(setopt package-enable-at-startup nil)

;; Redirect native compilation cache if possible.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(let ((minver "30.1"))
  (when (version< emacs-version minver)
    (error "The minimum Emacs version expected is %s" minver)))

(setopt ok-debug nil                    ; global switch for debugging
        debug-on-error ok-debug

        confirm-kill-processes t
        inhibit-default-init nil
        native-comp-async-query-on-exit t
        pgtk-wait-for-event-timeout 0)
(setq debug-on-message nil                    ; set regexp to trigger debugger
      byte-compile-warnings '(not obsolete))  ; set t for development

;; Reduce GC usage while initialization. 800 kb is the default (2021-08-01).
;; Note that the threshold while running is set by gcmh later in init and the
;; following temporary setting will be overridden. Use that for adjustment.
(setopt gc-cons-threshold most-positive-fixnum)

;; UI:
(setopt frame-inhibit-implied-resize t
        inhibit-startup-screen t
        initial-buffer-choice nil
        native-comp-async-report-warnings-errors ok-debug
        ring-bell-function 'ignore)
(setq native-comp-jit-compilation t
      redisplay-skip-fontification-on-input t)

(dolist (it '((menu-bar-lines . 0)
              (tool-bar-lines . 0)
              ;; (width . 120)  ; default frame width
              ;; (height . 40)  ; default frame height
              (vertical-scroll-bars . nil)))
  (push it default-frame-alist))

(fringe-mode '(12 . 12))

;; Misc. configurations
(setenv "LSP_USE_PLISTS" "true")

;; Ignore x session resources:
(advice-add 'x-apply-session-resources :override 'ignore)

;; Disable magic file name during `init.el'
(defconst ok--saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            "Restore `file-name-handler-alist'."
            (setq file-name-handler-alist ok--saved-file-name-handler-alist)
            (unintern 'ok--saved-file-name-handler-alist obarray))
          98)

;; Profile `init.el'.
(when ok-debug  ; activate profiler in debug mode
  (profiler-start 'cpu+mem)
  (add-hook 'after-init-hook
            (lambda ()
              (message "Emacs (PID:%d) started in %s"
                       (emacs-pid) (emacs-init-time))
              (profiler-report)
              (profiler-stop))
            99))

;; Configure package manager. (`init-straight.el' or `init-package.el')
(load (locate-user-emacs-file "lisp/init-straight.el"))

;;; early-init.el ends here
