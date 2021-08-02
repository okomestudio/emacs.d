;;; init-sql.el --- SQL  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sql
  :config
  (sql-set-product 'postgres))

(use-package sqlformat
  :after (sql)

  :bind
  (:map sql-mode-map
   ("C-M-b" . 'ts/sqlformat))

  :custom
  (sqlformat-command 'sqlfluff)
  ;(sqlformat-command 'pgformatter)
  ;(sqlformat-args '("-f2" "-g" "-s4" "-U2"
  ;                  "-M" "-p" "\n[ ]*-- sqlfmt: off\n(?:.*)?-- sqlfmt: on\n"))

  :ensure-system-package
  ;(pg_format . "sudo apt install pgformatter")
  ("~/.pyenv/shims/sqlfluff" . "~/.pyenv/shims/pip3 install sqlfluff")

  ;:hook
  ;((sql-mode . sqlformat-on-save-mode))

  :init
  (require 'sqlformat)

  (defun ts/sqlformat ()
    (interactive)
    (save-excursion
      (sqlformat (point-min) (point-max))
      (delete-trailing-whitespace))))

(use-package sql-upcase
  :disabled t

  :ensure nil

  :init
  (ensure-file-from-github "emacsmirror/emacswiki.org/master/sql-upcase.el")

  :hook
  ((sql-mode sql-interactive-mode) . sql-upcase-mode))

(provide 'init-sql)
;;; init-sql.el ends here
