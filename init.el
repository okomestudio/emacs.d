;;; emacs --- Emacs configuration  -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file should be placed at ~/.config/emacs/init.el.
;;
;;
;;; Code:

;; INIT.EL PROFILING
(defconst ts/profile-init nil "Set to t to profile init.el.")

(when ts/profile-init
  (require 'profiler)
  (profiler-start 'cpu)
  (defun ts/tear-down-profiler ()
    (profiler-report)
    (profiler-stop))
  (add-hook 'after-init-hook #'ts/tear-down-profiler))


;; custom.el is for variables configured interactively.
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(when (file-exists-p custom-file)
  (load custom-file))


;; UTILITY VARIABLES AND FUNCTIONS
(defconst ts/lisp-dir (expand-file-name "lisp" user-emacs-directory)
  "Path to the Lisp file directory.")
(defconst ts/site-lisp-dir (expand-file-name "site-lisp" ts/lisp-dir)
  "Path to the external Lisp file directory.")

(add-to-list 'load-path ts/lisp-dir)
(add-to-list 'load-path ts/site-lisp-dir)

(require 'init-utils)


;; PACKAGE CONFIGURATION
(require 'package)

(defvar package-archives)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; quelpa - https://github.com/quelpa/quelpa
(use-package quelpa-use-package
  :demand t
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  (unless (package-installed-p 'quelpa-use-package)
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://github.com/quelpa/quelpa-use-package.git"))))

(eval-and-compile
  (setq use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 7)
  (auto-package-update-maybe))


;; LOAD MODULES UNDER LISP DIRECTORY
(require 'init-startup)
(require 'init-themes)

(require 'init-ace)
(require 'init-anki)
(require 'init-ansible)
(require 'init-bookmark-plus)
(require 'init-c)
(require 'init-company)
(require 'init-dash)
(require 'init-dired)
(require 'init-docker)
(require 'init-edit-server)
(require 'init-editing-utils)
(require 'init-elisp)
(require 'init-faces)
(require 'init-flycheck)
(require 'init-git)
(require 'init-gnus)
(require 'init-google-translate)
(require 'init-graphviz)
(require 'init-helm)
(require 'init-ini)
(require 'init-json)
(require 'init-keychain)
(require 'init-lsp)
(require 'init-markdown)
(require 'init-minibuffer)
(require 'init-multiple-cursors)
(require 'init-openwith)
(require 'init-org)
(require 'init-plantuml)
(require 'init-prog)
(require 'init-projectile)
(require 'init-python)
(require 'init-restclient)
(require 'init-rst)
(require 'init-rust)
(require 'init-scala)
(require 'init-shell)
(require 'init-sql)
(require 'init-tern)
(require 'init-tramp)
(require 'init-treemacs)
(require 'init-vterm)
(require 'init-webdev)
(require 'init-windows)
(require 'init-yaml)
(require 'init-yascroll)
(require 'init-yasnippet)


;; If other init files exist, load them all.
(when (file-exists-p (concat user-emacs-directory "init.d/"))
  (mapc (lambda (f) (load f))
        (directory-files (concat user-emacs-directory "init.d/") t ".el$")))


(provide 'init)
;;; init.el ends here
