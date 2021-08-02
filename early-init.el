;;; early-init.el

;;; Commentary:

;; Provides early initialization for Emacs > 27.1.

;;; Code:

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode t)

;; (setq package-quickstart t)

(setq frame-inhibit-implied-resize t)

(provide 'early-init)

;;; early-init ends here
