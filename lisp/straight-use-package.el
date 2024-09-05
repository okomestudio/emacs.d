;;; straight-use-package.el --- use-package with straight  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize Straight for use with `use-package'.
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
;;; Code:

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

(setopt straight-use-package-by-default t

        use-package-always-defer t               ; use :demand t to override
        use-package-compute-statistics ok-debug  ; for use-package-report
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.001
        use-package-verbose ok-debug

        message-log-max t)

(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)

;;; straight-use-package.el ends here
