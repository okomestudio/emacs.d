;;; init-tramp.el --- Tramp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tramp
  :defer t

  :custom
  (tramp-default-method "ssh"))

(provide 'init-tramp)
;;; init-tramp.el ends here
