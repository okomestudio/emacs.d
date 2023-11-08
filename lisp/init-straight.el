;;; init-straight.el --- Straight  -*- lexical-binding: t -*-
;;; Commentary:
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

(provide 'init-straight)
;;; init-straight.el ends here
