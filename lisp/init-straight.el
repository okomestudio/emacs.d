;;; init-straight.el --- Straight  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	      (url-retrieve-synchronously
	       "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)
(setq straight-use-package-by-default t)

;; (use-package auto-package-update
;;   :config
;;   (auto-package-update-maybe)
;;   :custom
;;   (auto-package-update-delete-old-versions t)
;;   (auto-package-update-interval 7))

(provide 'init-straight)
;;; init-straight.el ends here
