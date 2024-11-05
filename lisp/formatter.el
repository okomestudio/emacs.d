;;; formatter.el --- formatter  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The code formatter configuration.
;;
;;; Code:

(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier")e)

;;; formatter.el ends here
