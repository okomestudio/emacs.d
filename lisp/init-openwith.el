;;; init-openwith.el --- Openwith  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package openwith
  ;; Associate external applications with files.
  :custom
  (openwith-associations '(("\\.pdf\\'" "okular" (file))))

  :init
  (openwith-mode t))


(provide 'init-openwith)
;;; init-openwith.el ends here
