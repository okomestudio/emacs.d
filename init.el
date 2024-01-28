;;; init.el --- Emacs startup configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Main init.el.
;;
;;; Code:

;; Profiling
(when nil ;; set t to activate init profiler
  (require 'profiler)
  (profiler-start 'cpu)
  (defun init--tear-down-profiler ()
    (profiler-report)
    (profiler-stop))
  (add-hook 'after-init-hook #'init--tear-down-profiler))

(add-to-list 'after-init-hook
             (lambda ()
               (message "Emacs (pid:%d) started in %s"
                        (emacs-pid) (emacs-init-time))))

;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(when (file-exists-p custom-file)
  (load custom-file))

;; Configure use-package, init-straight.el or init-package.el:
(load-file (expand-file-name "init.d/init-straight.el" user-emacs-directory))

;; Load more config files from init.d
(use-package init-loader
  :custom
  (init-loader-default-regexp "\\(?:\\`[[:digit:]]\\{2\\}-\\).*[^-][^X].el\\'")
  (init-loader-show-log-after-init t)

  :config
  (init-loader-load (expand-file-name "init.d" user-emacs-directory)))

;;; init.el ends here
