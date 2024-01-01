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


;; RST

(use-package rst-mode
  :straight nil

  :ensure-system-package
  (sphinx-quickstart . "pip install sphinx")

  :mode
  "\\.rst\\'")


(provide 'init-text-mode)
;;; init-text-mode.el ends here
