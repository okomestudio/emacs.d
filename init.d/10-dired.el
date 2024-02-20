;;; 10-dired.el --- dired  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Dired and related utilities.
;;
;;; Code:

(use-package dired
  :straight nil
  :config (require 'dired-x)
  :custom
  ;; (dired-omit-files "^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.pyo$\\|\#$")
  (dired-listing-switches "-alh"))

;;; 10-dired.el ends here
