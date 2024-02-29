;;; init.el --- Emacs startup configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Main init.el.
;;
;;; Code:

;; Disable magic file name during init.
(defconst ok--saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            "Restore magic file name handers."
            (setq file-name-handler-alist
                  ok--saved-file-name-handler-alist)) 99)

;; Profiling
(when ok-debug ;; set t to activate init profiler
  (profiler-start 'cpu+mem)
  (defun init--tear-down-profiler ()
    (profiler-report)
    (profiler-stop))
  (add-hook 'after-init-hook #'init--tear-down-profiler))

(add-hook 'after-init-hook
          (lambda ()
            (message "Emacs (pid:%d) started in %s" (emacs-pid) (emacs-init-time)))
          100)

;; custom.el
(setq custom-file (locate-user-emacs-file "custom.el")) ;; or use `null-device'
(load custom-file 'noerror 'nomessage)

;; Configure use-package, init-straight.el or init-package.el:
(load (expand-file-name "init.d/init-straight.el" user-emacs-directory))

;; Load more config files from init.d
(use-package init-loader
  :demand t
  :custom
  ;; Use this to load specific init.d file for debugging:
  ;; (init-loader-default-regexp
  ;;  (concat "\\`\\(?:[[:digit:]]\\{2\\}-\\)\\("
  ;;          (mapconcat #'identity '("foo" "bar") "\\|")
  ;;          "\\)\\.el\\'"))

  ;; NOTE: regexp is case-insensitive
  (init-loader-default-regexp
   "\\`\\(?:[[:digit:]]\\{2\\}-\\)\\([a-z-]+\\(-[a-wyz-]\\|[a-z][a-z-]\\)\\).el\\'")

  (init-loader-show-log-after-init ok-debug)

  :config
  (init-loader-load (expand-file-name "init.d" user-emacs-directory)))

;;; init.el ends here
