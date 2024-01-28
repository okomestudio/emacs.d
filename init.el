;;; emacs --- Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Main init.el. Place this at a path that Emacs can find.
;;
;;; Code:

;; init profiling
(defconst ts/profile-init nil
  "Set to t to profile init.el.")

;; suppress some warnings (set t while development)
(setq byte-compile-warnings '(not obsolete))


(when ts/profile-init
  (require 'profiler)
  (profiler-start 'cpu)
  (defun init--tear-down-profiler ()
    (profiler-report)
    (profiler-stop))
  (add-hook 'after-init-hook #'init--tear-down-profiler))


;; custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(when (file-exists-p custom-file)
  (load custom-file))


;; load-path
(defconst ts/lisp-dir (expand-file-name "lisp" user-emacs-directory)
  "Path to the Lisp file directory.")
(defconst ts/site-lisp-dir (expand-file-name "site-lisp" ts/lisp-dir)
  "Path to the external Lisp file directory.")

(add-to-list 'load-path ts/lisp-dir)
(add-to-list 'load-path ts/site-lisp-dir)


(defgroup ts nil
  "TS personal customization group.")


;; Configure packages
(require 'init-straight) ;; also configures use-package
;; (require 'init-package) ;; uncomment this instead for use-package without straight

;; Load additional init.el files in init.d/.
(use-package init-loader
  :custom
  (init-loader-default-regexp "\\(?:\\`[[:digit:]]\\{2\\}-\\)[^X]+\\'")
  (init-loader-show-log-after-init t)

  :config
  (init-loader-load (expand-file-name "init.d" user-emacs-directory)))


(add-to-list 'after-init-hook
             (lambda ()
               (message "Emacs (pid:%d) started in %s"
                        (emacs-pid) (emacs-init-time))))

(put 'eval 'safe-local-variable #'listp)

;;; init.el ends here
