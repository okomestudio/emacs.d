;;; 50-text-mode.el --- text-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package text-mode
  :straight nil
  :hook
  (before-save . (lambda ()
                   (when (derived-mode-p 'text-mode)
                     (save-excursion
                       (delete-trailing-whitespace))))))

;;; 50-text-mode.el ends here
