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

(eval-and-compile
  (setq use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package)

;; quelpa - Build and install your Emacs Lisp packages on-the-fly directly from source.
;; https://github.com/quelpa/quelpa
(unless (package-installed-p 'quelpa)
  (package-install 'quelpa))

(use-package quelpa-use-package
  :demand t
  :init
  (setq quelpa-dir (expand-file-name ".quelpa" user-emacs-directory)
        quelpa-use-package-inhibit-loading-quelpa t)
  (unless (package-installed-p 'quelpa-use-package)
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://github.com/quelpa/quelpa-use-package.git"))))

(use-package auto-package-update
  :config
  (auto-package-update-maybe)

  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 7))


;; LOAD MODULES UNDER LISP DIRECTORY

;; essentials
(require 'init-startup)
(require 'init-faces)
(require 'init-themes)
(require 'init-minibuffer)
(require 'init-treemacs)

(require 'init-editing-utils)

(require 'init-bookmark-plus)
(require 'init-company)
(require 'init-docker)
(require 'init-edit-server)
(require 'init-elfeed)
(require 'init-git)
(require 'init-google-translate)
(require 'init-lsp)
(require 'init-openwith)
(require 'init-projectile)
(require 'init-vterm)
;; (require 'init-anki)
;; (require 'init-dired)
;; (require 'init-gnus)
;; (require 'init-tern)
;; (require 'init-yasnippet)

;; file formats
(require 'init-file-type-modes)

;; programming languages and dev frameworks
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

;; org
(require 'init-org)


;; LOAD CUSTOM INIT.EL
(let ((custom-init-directory (concat user-emacs-directory "init.d/")))
  (when (file-exists-p custom-init-directory)
    (mapc (lambda (f) (load f))
          (directory-files custom-init-directory t ".el$"))))

(provide 'init)
;;; init.el ends here
