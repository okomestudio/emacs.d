;;; 50-text-mode.el --- text-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; text-mode configuration.
;;
;;; Code:

(use-package text-mode
  :straight nil
  :hook (before-save . text-mode-ok-clean-up-whitespaces)
  :config
  (defun text-mode-ok-clean-up-whitespaces ()
    (when (derived-mode-p 'text-mode)
      (save-excursion
        (delete-trailing-whitespace)))))

;;; CSV

(use-package csv-mode)

(use-package rainbow-csv
  ;; Highlight CSV and TSV spreadsheet files in different rainbow colors.
  :straight (rainbor-csv
             :type git
             :host github
             :repo "emacs-vs/rainbow-csv"))

;;; TOML

(use-package toml-ts-mode
  :straight nil
  :custom (toml-ts-mode-indent-offset 4))

;;; 50-text-mode.el ends here
