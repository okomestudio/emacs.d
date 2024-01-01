;;; init-prog-mode.el --- prog-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package prog-mode
  :straight nil
  :hook
  (prog-mode . (lambda ()
                 (add-hook 'local-write-file-hooks
                           #'(lambda () (save-excursion
                                          (delete-trailing-whitespace))))
                 (show-paren-mode))))


(provide 'init-prog-mode)
;;; init-prog-mode.el ends here
