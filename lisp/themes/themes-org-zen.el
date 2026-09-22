;;; themes-org-zen.el --- Org Zen Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; URL: https://github.com/okomestudio/org-zen-theme
;;
;;; Code:

;; (require 'ok)

(use-package org-zen-theme
  :straight (org-zen-theme
             :type git :host github :repo "okomestudio/org-zen-theme")
  :config
  (load-theme 'org-zen-dark t t)
  (load-theme 'org-zen-light t t))

(provide 'themes-org-zen)
;;; themes-org-zen.el ends here
