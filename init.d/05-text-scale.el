;;; 05-text-scale.el --- Text scaling  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :straight nil

  :hook
  (elfeed-search-mode . (lambda () (text-scale-set 1.0)))
  (elfeed-show-mode . (lambda () (text-scale-set 1.5)))
  (eww-mode . (lambda () (text-scale-set 1.5)))
  (org-mode . (lambda () (text-scale-set 1.5)))
  (prog-mode . (lambda () (text-scale-set 0.5)))
  (text-mode . (lambda () (text-scale-set 0.5)))
  (treemacs-mode . (lambda () (text-scale-decrease 0.4)))

  (text-scale-mode
   . (lambda ()
       (when (derived-mode-p 'org-mode)
         (let ((factor (expt text-scale-mode-step text-scale-mode-amount)))
           (plist-put org-format-latex-options :scale (* 3.33 factor)))))))

;;; 05-text-scale.el ends here
