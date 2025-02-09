;;; subsys-git.el --- Git Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Git subsystem.
;;
;;; Code:

(use-package magit  ; not derived from prog-mode
  ;; TODO: Bind `magit-find-file'
  :custom ((magit-format-file-function #'magit-format-file-with-nerd-icon))
  :config
  (defun magit-format-file-with-nerd-icon (_kind file face &optional status orig)
    "Use in place of `magit-format-file-default'."
    (require 'nerd-icons)
    (propertize
     (let ((icon (nerd-icons-icon-for-file file)))
       (concat (and status (format "%-11s" status))
               (if orig
                   (format "%s -> %s %s" orig icon file)
                 (format "%s %s" icon file))))
     'font-lock-face face)))

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
