;;; init-use-package.el --- use-package Initialization  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setopt use-package-always-defer t ; use :demand t to override
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.001)

(require 'use-package)
(require 'use-package-ensure-system-package)

(use-package system-packages
  :custom ((system-packages-use-sudo t)
           (system-packages-package-manager 'apt)))

(provide 'init-use-package)
;;; init-use-package.el ends here
