;;; emacs --- Emacs configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; init profiling
(defconst ts/profile-init nil "Set to t to profile init.el.")

;; suppress some warnings (set t while development)
(setq byte-compile-warnings '(not obsolete))


(when ts/profile-init
  (require 'profiler)
  (profiler-start 'cpu)
  (defun ts/tear-down-profiler ()
    (profiler-report)
    (profiler-stop))
  (add-hook 'after-init-hook #'ts/tear-down-profiler))


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
;; (require 'init-package) ;; use this instead for non-straight use-package

(require 'init-utils)
(require 'dir-locals-utils)
(require 'init-org)
(require 'init-org-roam)
(require 'init-startup)
(require 'init-help)
(require 'init-wayland)
(require 'init-auth-source)
(require 'init-themes)
(require 'init-faces)
(require 'init-text-scale)
(require 'init-minibuffer)
(require 'init-treemacs)
(require 'init-consult)
(require 'init-hydra)
(require 'init-popper)

(require 'init-editing-utils)
(require 'init-flycheck)
(require 'init-eww)
(require 'init-lookup)
(require 'init-ime)
(require 'init-pbocr)
(require 'init-pdf)

(require 'init-anki)
(require 'init-bookmark-plus)
(require 'init-company)
(require 'init-docker)
(require 'init-elfeed)
(require 'init-git)
(require 'init-lsp)
(require 'init-openwith)

;; (require 'init-dired)
(require 'init-gnus)
;; (require 'init-tern)
(require 'init-yasnippet)

(require 'init-file-type-modes)

(require 'init-c)
(require 'init-elisp)
(require 'init-kotlin)
(require 'init-python)
(require 'init-rust)
(require 'init-scala)
(require 'init-shell)
(require 'init-sql)
(require 'init-webdev)

(require 'init-ansible)
(require 'init-graphviz)
(require 'init-plantuml)
(require 'init-restclient)

(require 'init-ai)
(require 'init-osm)
(require 'init-slack)

(require 'init-projectile)

(require 'init-polymode)

(require 'init-vterm)
(require 'init-eat)

;; (require 'init-eaf)

(require 'init-optimizations)

(require 'init-tetris)

;; Load additional init.el files in init.d/:
(let ((custom-init-directory (concat user-emacs-directory "init.d/")))
  (when (file-exists-p custom-init-directory)
    (mapc (lambda (f) (load f))
          (directory-files custom-init-directory t ".el$"))))

(put 'eval 'safe-local-variable #'listp)
(setq gc-cons-threshold 100000000)

(provide 'init)
;;; init.el ends here
