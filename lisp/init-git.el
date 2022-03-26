;;; init-git.el --- Git  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; magit.el - An interface to Git.
;; https://github.com/magit/magit
(use-package magit
  :init
  ;; workaround for https://github.com/magit/magit/pull/4445
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil)))

(provide 'init-git)
;;; init-git.el ends here
