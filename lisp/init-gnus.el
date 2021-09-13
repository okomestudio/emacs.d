;;; init-gnus.el --- Gnus  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gnus
  :custom (gnus-summary-line-format "%U%R%z%I%(%[%o: %-23,23f%]%) %s\\n")

  :hook ((gnus-summary-mode . (lambda () (run-with-idle-timer 0.1 nil 'ts/gnus-show-all))))

  :init
  (setq gnus-home-directory "~/.config/gnus/")
  (setq gnus-init-file (concat gnus-home-directory "gnus.el"))
  (setq gnus-directory (concat gnus-home-directory "news/"))
  (setq message-directory (concat gnus-home-directory "mail/"))
  (setq nnfolder-directory (concat gnus-home-directory "mail/archive/"))
  (setq gnus-dribble-directory gnus-home-directory)

  :config
  (defun ts/gnus-show-all ()
    (interactive)
    (gnus-summary-insert-old-articles t)) )

(provide 'init-gnus)
;;; init-gnus.el ends here
