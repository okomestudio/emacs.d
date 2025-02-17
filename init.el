;;; init.el --- Emacs startup configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This runs after `early-init.el'.
;;
;;; Code:

;; Disable magic file name during init
(defconst ok--saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            "Restore magic file name handers."
            (setq file-name-handler-alist
                  ok--saved-file-name-handler-alist)) 99)

;; Profile `init.el'
(when ok-debug  ; profiler active when debug mode is active
  (profiler-start 'cpu+mem)
  (defun init--tear-down-profiler ()
    (profiler-report)
    (profiler-stop))
  (add-hook 'after-init-hook #'init--tear-down-profiler))

(add-hook 'after-init-hook
          (lambda ()
            (message "Emacs (pid:%d) started in %s"
                     (emacs-pid) (emacs-init-time)))
          100)

;; Disable `custom.el'
(setopt custom-file null-device)

;; Use `init-straight.el' or `init-package.el'
(load (locate-user-emacs-file "lisp/init-straight.el"))

(use-package no-littering
  ;; Run this as early as possible.
  :demand t
  :custom ((no-littering-etc-directory
            (locate-user-emacs-file (convert-standard-filename "etc/")))
           (no-littering-var-directory
            (locate-user-emacs-file (convert-standard-filename "var/")))))

;; Load config files under `init.d/'
(use-package init-loader
  :demand t
  :custom ((init-loader-byte-compile nil)
           (init-loader-show-log-after-init ok-debug))
  :config
  (init-loader-load (locate-user-emacs-file "init.d")))

;;; init.el ends here
