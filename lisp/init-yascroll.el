;;; init-yascroll.el --- Yascroll  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yascroll
  :init
  (ensure-file-from-github "emacsorphanage/yascroll/master/yascroll.el")
  (when (not window-system)
    (require 'yascroll)
    (global-yascroll-bar-mode 1)))

(provide 'init-yascroll)
;;; init-yascroll.el ends here
