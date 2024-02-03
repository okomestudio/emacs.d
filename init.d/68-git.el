;;; 68-git.el --- Git  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit :defer t)  ;; not derived from prog-mode


(use-package devdocs
  :defer t

  :hook
  (magit-mode . (lambda () (setq-local devdocs-current-docs '("git")))))

;;; 68-git.el ends here
