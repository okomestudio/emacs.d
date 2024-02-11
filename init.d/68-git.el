;;; 68-git.el --- Git  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit)  ;; not derived from prog-mode


(use-package devdocs
  :hook
  (magit-mode . (lambda () (setq-local devdocs-current-docs '("git")))))

;;; 68-git.el ends here
