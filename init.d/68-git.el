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

(use-package blamer
  ;; Git blame plugin.
  :straight (:host github :repo "artawower/blamer.el")
  :bind (("s-b" . blamer-show-posframe-commit-info))

  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)

  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 100
                   :italic t))))

;;; 68-git.el ends here
