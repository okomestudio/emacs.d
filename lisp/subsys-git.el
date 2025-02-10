;;; subsys-git.el --- Git Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Git subsystem.
;;
;;; Code:

(use-package magit  ; not derived from prog-mode
  ;; TODO: Bind `magit-find-file'
  :custom ((magit-format-file-function #'magit-format-file-nerd-icons)))

(use-package magit-todos
  :after magit
  :hook (magit-mode . (lambda () (magit-todos-mode 1)))
  :config
  (dolist (kw '("WIP"
                "TODO [#A]" "TODO [#B]" "TODO [#C]"
                "WIP [#A]" "WIP [#B]" "WIP [#C]"))
    (add-to-list 'magit-todos-keywords-list kw)))

(use-package blamer
  ;; Git blame plugin.
  :straight (:host github :repo "artawower/blamer.el")
  :bind (("s-b" . blamer-show-posframe-commit-info))
  :custom ((blamer-idle-time 0.3)
           (blamer-min-offset 70))
  :custom-face (blamer-face ((t :foreground "#7a88cf"
                                :background nil
                                :height 100
                                :italic t))))

(provide 'subsys-git)
;;; subsys-git.el ends here
