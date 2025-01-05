;;; subsys-writing-en.el --- English Writing Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the English writing subsystem.
;;
;;; Code:

(use-package writegood-mode
  :disabled                             ; use flycheck integration
  :hook ((org-mode . writegood-mode)))

(provide 'subsys-writing-en)
;;; subsys-writing-en.el ends here
