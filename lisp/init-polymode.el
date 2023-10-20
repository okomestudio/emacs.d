;;; init-polymode.el --- Polymode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package polymode
  :mode
  ("\.py$" . poly-python-sql-mode)

  :config
  (setq polymode-prefix-key (kbd "M-n"))
  (define-hostmode poly-python-hostmode :mode 'python-mode)

  (define-innermode poly-sql-expr-python-innermode
    :mode 'sql-mode
    :head-matcher "\"\\{3\\}--[[:blank:]]*\\(sql\\|SQL\\)"
    :tail-matcher "\"\\{3\\}"
    :head-mode 'host
    :tail-mode 'host)

  (defun poly-python-sql-eval-chunk (beg end msg)
    "Calls out to `sql-send-region' with the polymode chunk region"
    (sql-send-region beg end))

  (define-polymode poly-python-sql-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-sql-expr-python-innermode)
    (setq polymode-eval-region-function #'poly-python-sql-eval-chunk)
    (define-key poly-python-sql-mode-map
                (kbd "C-c C-c") 'polymode-eval-chunk)))


(provide 'init-polymode)
;;; init-polymode.el ends here
