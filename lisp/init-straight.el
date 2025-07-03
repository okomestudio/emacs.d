;;; init-straight.el --- Straight Initialization  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize `straight' for use with `use-package'.
;;
;; To upgrade a package and its dependencies:
;;
;;   M-x straight-pull-package-and-deps <package>
;;   M-x straight-freeze-version
;;
;; The frozen versions will be saved to the
;; straight/versions/default.el file. Put this file under version
;; control.
;;
;; When Straight complains that a package is missing, run
;;
;;   M-x straight-pull-recipe-repositories
;;
;; to update recipe repositories.
;;
;; The following utility functions may be useful:
;;
;;   - `straight-visit-package' to browse the package source code
;;   - `straight-visit-package-website' to visit the package website
;;
;; Depending on the `straight-check-for-modifications' setting, the
;; explicit execution of
;;
;;   - M-x straight-rebuild-package
;;
;; on package modification may be necessary.
;;
;;; Code:

;; The modification check by `straight' is minimized by
;;
;;   - removing the `find-at-startup' option
;;   - using the `watch-files' option
;;
;; For the latter, github.com/watchexec/watchexec is required.
;;
(setopt straight-check-for-modifications '(watch-files))

(setopt straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file (locate-user-emacs-file
                       "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 7))  ; straight bootstrap version
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	      (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/"
                 "radian-software/straight.el/develop/install.el")
	       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setopt use-package-always-defer t               ; use :demand t to override
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.001
        message-log-max t)

(require 'use-package)
(require 'use-package-ensure-system-package)

(use-package system-packages
  :custom ((system-packages-use-sudo t)
           (system-packages-package-manager 'apt)))

;; Ensure use of builtin version for the following packages:
(use-package project :straight (:type built-in))

(provide 'init-straight)
;;; init-straight.el ends here
