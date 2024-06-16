;;; 68-git.el --- Git  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Git related utilities.
;;
;;; Code:

(use-package magit ;; not derived from prog-mode
  )

(use-package magit-file-icons
  :after magit
  :hook (magit-mode . (lambda () (magit-file-icons-mode 1))))

(use-package magit-todos
  :after magit
  :hook (magit-mode . (lambda () (magit-todos-mode 1))))

;;; 68-git.el ends here
