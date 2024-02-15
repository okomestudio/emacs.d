;;; 60-prog-mode.el --- prog-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure prog-mode related utilities.
;;
;;; Code:

(use-package prog-mode
  :straight nil
  :hook
  (before-save . (lambda ()
                   (when (derived-mode-p 'prog-mode)
                     (save-excursion
                       (delete-trailing-whitespace))))))

;;; 60-prog-mode.el ends here
