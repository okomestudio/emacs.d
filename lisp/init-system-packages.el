;;; init-system-packages.el --- system-packages  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package system-packages
  :custom
  ((system-packages-use-sudo t)
   (system-packages-package-manager 'apt)))

(provide 'init-system-packages)
;;; init-system-packages.el ends here
