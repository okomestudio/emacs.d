;;; maj-text-mode.el --- Text Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Text major mode.
;;
;;; Code:

;; (use-package text-mode)

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
