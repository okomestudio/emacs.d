;;; init-openwith.el --- Openwith  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package openwith
  ;; Associate external applications with files.

  :custom
  (openwith-associations '(("\\.pdf\\'" "okular" (file))))

  :config
  (openwith-mode -1)

  (with-eval-after-load 'mm-util
    (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)))


(provide 'init-openwith)
;;; init-openwith.el ends here
