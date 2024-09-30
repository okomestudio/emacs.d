;;; faces-inactive.el --- faces  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Font configuration.
;;
;;; Code:

;; ICONS

(use-package all-the-icons
  :if (and (display-graphic-p)
           (member system-type '(gnu gnu/linux gnu/kfreebsd)))
  :config
  (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
    (all-the-icons-install-fonts +1)))

;; MISC.

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :custom (mixed-pitch-variable-pitch-cursor nil)
  :config
  (delete 'org-table mixed-pitch-fixed-pitch-faces)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'font-lock-comment-face)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-modern-bracket-line)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-modern-tag))

;;; faces-inactive.el ends here
