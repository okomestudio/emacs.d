;;; emacs --- Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:
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

(require 'init-org)
(require 'init-org-roam)
(require 'init-startup)
(require 'init-navigation)
(require 'init-minibuffer)
(require 'init-help)
(require 'init-wayland)
(require 'init-auth-source)
(require 'init-visuals)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-ime)
(require 'init-terminal)
(require 'init-editing)
(require 'init-dir-locals)
(require 'init-lookup)
(require 'init-text-mode)
(require 'init-conf-mode)
(require 'init-prog)
(require 'init-file-viewers)
(require 'init-web-browsing)
(require 'init-ai)
(require 'init-games)
(require 'init-optimizations)


;; Load additional init.el files in init.d/:
(let ((custom-init-dir (expand-file-name "init.d" user-emacs-directory)))
  (when (file-exists-p custom-init-dir)
    (mapc (lambda (f) (load f))
          (directory-files custom-init-dir t ".el$"))))

(put 'eval 'safe-local-variable #'listp)
(setq gc-cons-threshold 100000000)

(provide 'init)
;;; init.el ends here
