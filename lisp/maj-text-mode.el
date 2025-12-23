;;; maj-text-mode.el --- Text  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure `text-mode' and related utilities.
;;
;;; Code:

;;; Folding

(use-package kirigami
  ;; A unified method to fold and unfold text.
  ;;
  ;; Commands exposed:
  ;;
  ;; - `kirigami-open-fold'
  ;; - `kirigami-open-fold-rec'
  ;; - `kirigami-close-fold'
  ;; - `kirigami-open-folds'
  ;; - `kirigami-close-folds'
  ;; - `kirigami-toggle-fold'
  ;;
  ;; NOTE(2025-12-22): See if these commands are useful for consolidating any
  ;; aspect of text folding. If not, remove.
  )

;;; CSV

(use-package csv-mode)

(use-package rainbow-csv
  ;; Highlight CSV and TSV spreadsheet files in different rainbow colors.
  )

;;; TOML

(use-package toml-ts-mode
  :custom (toml-ts-mode-indent-offset 4))

(provide 'maj-text-mode)
;;; maj-text-mode.el ends here
