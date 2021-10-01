;;; emacs --- Emacs configuration  -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file should be placed at ~/.config/emacs/init.el.
;;
;;
;;; Code:

;; Uncomment for profiling (also see the end of file)
;; (require 'profiler)
;; (profiler-start 'cpu)


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
(unless (package-installed-p 'quelpa-use-package)
  (package-install 'quelpa-use-package))
(require 'quelpa-use-package)

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
(require 'init-webdev)
(require 'init-yaml)
(require 'init-yascroll)
(require 'init-yasnippet)

;; Uncomment for profiling
;; (profiler-report)
;; (profiler-stop)

(provide 'init)
;;; init.el ends here
