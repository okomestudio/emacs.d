;;; pkg-dired.el --- Dired Setup  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up dired.
;;
;;; Code:

(use-package dired
  :straight nil
  :config (require 'dired-x)
  :custom ((dired-listing-switches "-alh")
           ;; (dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")
           ))

(provide 'pkg-dired)
;;; pkg-dired.el ends here
