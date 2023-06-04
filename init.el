;;; emacs --- Emacs configuration  -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; This file should be placed at ~/.config/emacs/init.el.
;;
;;
;;; Code:

;; Profiling init.el.
(defconst ts/profile-init nil "Set to t to profile init.el.")

(when ts/profile-init
  (require 'profiler)
  (profiler-start 'cpu)
  (defun ts/tear-down-profiler ()
    (profiler-report)
    (profiler-stop))
  (add-hook 'after-init-hook #'ts/tear-down-profiler))


;; Use sources under this directory for secrets.
(setq auth-sources '((:source "~/.config/emacs/secrets/.authinfo.gpg")))
(setq authinfo-hidden "\\(api_token\\|apikey\\|cookie\\|secret\\|passphrase\\|password\\)")


;; custom.el is for variables configured interactively.
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(when (file-exists-p custom-file)
  (load custom-file))


;; Set up load-paths for local modules.
(defconst ts/lisp-dir (expand-file-name "lisp" user-emacs-directory)
  "Path to the Lisp file directory.")
(defconst ts/site-lisp-dir (expand-file-name "site-lisp" ts/lisp-dir)
  "Path to the external Lisp file directory.")

(add-to-list 'load-path ts/lisp-dir)
(add-to-list 'load-path ts/site-lisp-dir)


;; Load personal helper functions.
(require 'init-utils)


;; Configure package managing around straight and use-package.

;; (require 'init-package)
(require 'init-straight)


;; Configure modules.

;; org
(require 'init-org)

;; essentials
(require 'init-startup)
(require 'init-system-packages)
(require 'init-themes)
(require 'init-faces)
(require 'init-minibuffer)
(require 'init-treemacs)

(require 'init-editing-utils)
(require 'init-editing-lookup)
(require 'init-ime)

(require 'init-anki)
(require 'init-bookmark-plus)
(require 'init-company)
(require 'init-docker)
(require 'init-edit-server)
(require 'init-elfeed)
(require 'init-git)
(require 'init-lsp)
(require 'init-openwith)
(require 'init-projectile)
(require 'init-vterm)
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

;; miscellaneous packages
(require 'init-gpt)
(require 'init-slack)

;; Load custom per-site init.el files stored under the init.d directory.
(let ((custom-init-directory (concat user-emacs-directory "init.d/")))
  (when (file-exists-p custom-init-directory)
    (mapc (lambda (f) (load f))
          (directory-files custom-init-directory t ".el$"))))

(put 'eval 'safe-local-variable #'listp)

(provide 'init)
