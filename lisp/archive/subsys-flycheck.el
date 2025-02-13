;;; subsys-flycheck.el --- Flycheck Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Flycheck subsystem.
;;
;;; Code:

(use-package flycheck
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (docutils . "pip install docutils"))

;;; Write-good
(use-package flycheck
  ;; Configuration for `write-good'
  :ensure-system-package (write-good . "npm install -g write-good")
  :config
  (flycheck-define-checker write-good
    "The write-good prose checker."
    :command ("write-good" "--no-thereIs" "--parse" source-inplace)
    :standard-input nil
    :error-patterns ((warning
                      line-start
                      (file-name) ":" line ":" column ":" (message)
                      line-end))
    :modes (gfm-mode markdown-mode org-mode text-mode))
  (add-to-list 'flycheck-checkers 'write-good))

(provide 'subsys-flycheck)
;;; subsys-flycheck.el ends here
