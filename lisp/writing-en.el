;;; writing-en.el --- writing-en  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure helpers for English writing.
;;
;;; Code:

(use-package writegood-mode
  :hook ((org-mode . writegood-mode))
  :ensure-system-package (write-good . "npm install -g write-good"))

;;; writing-en.el ends here
