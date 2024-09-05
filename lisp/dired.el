;;; dired.el --- Dired  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Dired configuration.
;;
;;; Code:

(use-package dired
  :straight nil
  :config (require 'dired-x)
  :custom ((dired-listing-switches "-alh")
           ;; (dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")
           ))

;;; dired.el ends here
