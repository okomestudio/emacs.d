;;; init-git.el --- Git  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package magit)


(use-package devdocs
  :hook
  (magit-mode . (lambda () (setq-local devdocs-current-docs '("git")))))


(provide 'init-git)
;;; init-git.el ends here
