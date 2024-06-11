;;; init-straight.el --- Straight  -*- lexical-binding: t -*-
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

(defvar bootstrap-version
  "Straight bootstrap version.")
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	      (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/"
                 "radian-software/straight.el/develop/install.el")
	       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setopt straight-use-package-by-default t)
(setopt use-package-always-defer t ;; use :demand t to override
        use-package-compute-statistics ok-debug ;; for use-package-report
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.001
        use-package-verbose ok-debug)
(setopt message-log-max t)

(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)

;;; init-straight.el ends here
