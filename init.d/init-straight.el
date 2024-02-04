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

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (url (concat "https://raw.githubusercontent.com/"
                   "radian-software/straight.el/develop/install.el")))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	      (url-retrieve-synchronously url
	                                  'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)

(setq straight-use-package-by-default t)
(setq use-package-enable-imenu-support t)
(setq use-package-verbose t)
(setq use-package-compute-statistics t) ;; for use-package-report
(setq use-package-minimum-reported-time 0.01)
(setq message-log-max t)

;;; init-straight.el ends here
