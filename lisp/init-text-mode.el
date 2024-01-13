;;; init-text-mode.el --- text-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package text-mode
  :straight nil
  :hook
  (text-mode . (lambda ()
                 (add-hook 'local-write-file-hooks
                           #'(lambda () (save-excursion
                                          (delete-trailing-whitespace)))))))


(require 'init-markdown)
(require 'init-rst)
(require 'init-yaml)


(provide 'init-text-mode)
;;; init-text-mode.el ends here
