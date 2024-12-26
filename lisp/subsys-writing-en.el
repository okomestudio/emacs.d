;;; subsys-writing-en.el --- English Writing Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the English writing subsystem.
;;
;;; Code:

(use-package writegood-mode
  :hook ((org-mode . writegood-mode))
  :ensure-system-package (write-good . "npm install -g write-good"))

(provide 'subsys-writing-en)
;;; subsys-writing-en.el ends here
