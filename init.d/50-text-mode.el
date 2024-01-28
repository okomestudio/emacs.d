;;; 50-text-mode.el --- text-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package text-mode
  :straight nil
  :hook
  (text-mode . (lambda ()
                 (add-hook 'local-write-file-hooks
                           #'(lambda () (save-excursion
                                          (delete-trailing-whitespace)))))))

;;; 50-text-mode.el ends here
