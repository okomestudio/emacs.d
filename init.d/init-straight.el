;;; init-straight.el --- Straight  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; To upgrade a package and its dependencies:
;;
;;   M-x straight-pull-package-and-deps <package>
;;   M-x straight-freeze-version
;;
;; Save the straight/versions/default.el file.
;;
;; Use the straight-visit-package and straight-visit-package-website functions
;; to browse the package code base and the package website, respectively.
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

(setq straight-use-package-by-default t)
(setq use-package-always-defer t ;; use :demand t to override
      use-package-compute-statistics ok-debug ;; for use-package-report
      use-package-enable-imenu-support t
      use-package-minimum-reported-time 0.001
      use-package-verbose ok-debug)
(setq message-log-max t)

(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)

;;; init-straight.el ends here
