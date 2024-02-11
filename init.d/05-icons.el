;;; 05-icons.el --- Icons  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nerd-icons
  :if (member system-type '(gnu gnu/linux gnu/kfreebsd))
  :config
  (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
    (nerd-icons-install-fonts +1)))


(use-package all-the-icons
  :disabled
  :if (and (display-graphic-p)
           (member system-type '(gnu gnu/linux gnu/kfreebsd)))
  :config
  (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
    (all-the-icons-install-fonts +1)))

;;; 05-icons.el ends here
