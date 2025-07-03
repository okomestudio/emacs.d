;;; maj-text-mode.el --- Text Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Text major mode.
;;
;;; Code:

;; (use-package text-mode
;;   :straight (:type built-in))

;;; CSV

(use-package csv-mode)

(use-package rainbow-csv
  ;; Highlight CSV and TSV spreadsheet files in different rainbow colors.
  :straight (rainbor-csv :type git
                         :host github
                         :repo "emacs-vs/rainbow-csv"))

;;; TOML

(use-package toml-ts-mode
  :straight (:type built-in)
  :custom (toml-ts-mode-indent-offset 4))

(provide 'maj-text-mode)
;;; maj-text-mode.el ends here
