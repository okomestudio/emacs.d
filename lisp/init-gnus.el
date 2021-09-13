;;; init-gnus.el --- Gnus  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gnus
  :custom
  (gnus-home-directory "~/.config/gnus/")
  (gnus-init-file (concat gnus-home-directory "gnus.el"))
  (gnus-directory (concat gnus-home-directory "news/"))
  (message-directory (concat gnus-home-directory "mail/"))
  (nnfolder-directory (concat gnus-home-directory "mail/archive/"))
  (gnus-dribble-directory gnus-home-directory))

(provide 'init-gnus)
;;; init-gnus.el ends here
