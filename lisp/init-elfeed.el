;;; init-elfeed.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elfeed
  :hook (elfeed-show-mode . ts/elfeed-show-mode-hook)
  :init
  (defun ts/elfeed-show-mode-hook ()
    (setq-local shr-width nil
                shr-max-width nil)
    (text-scale-set 2.0)
    (toggle-truncate-lines +1)))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
