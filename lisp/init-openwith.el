;;; init-openwith.el --- Openwith  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; openwith.el - Associate external applications with files
(use-package openwith
  :custom ((openwith-associations '(("\\.pdf\\'" "okular" (file)))))
  :init
  (ensure-file-from-url "https://www.metalevel.at/misc/openwith.el")
  (openwith-mode t))

(provide 'init-openwith)
;;; init-openwith.el ends here
