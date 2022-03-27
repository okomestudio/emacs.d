;;; init-sql.el --- SQL  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sql
  :hook (sql-interactive-mode . (lambda () (setq truncate-lines t)))
  :config
  (sql-set-product 'postgres)

  (put 'sql-connection-alist 'safe-local-variable #'listp)
  (put 'sql-postgres-options 'safe-local-variable #'listp)
  (put 'sql-postgres-program 'safe-local-variable #'stringp))

(use-package sqlformat
  :after (sql)
  :bind (:map sql-mode-map ("C-c b" . ts/beautify-sql))

  ;; The sqlfluff version:
  :custom (sqlformat-command 'sqlfluff)
  :ensure-system-package ("~/.pyenv/shims/sqlfluff" . "~/.pyenv/shims/pip3 install sqlfluff")

  ;; The pgformatter version:
  ;; :custom
  ;; (sqlformat-command 'pgformatter)
  ;; (sqlformat-args '("-f2" "-g" "-s4" "-U2"
  ;;                   "-M" "-p" "\n[ ]*-- sqlfmt: off\n(?:.*)?-- sqlfmt: on\n"))
  ;; :ensure-system-package
  ;; (pg_format . "sudo apt install pgformatter")

  :init (require 'sqlformat)

  :config
  (defun ts/beautify-sql (beg end)
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (save-excursion
      (sqlformat beg end)
      (delete-trailing-whitespace))))

(use-package sql-upcase
  :disabled
  :ensure nil
  :init (ensure-file-from-github "emacsmirror/emacswiki.org/master/sql-upcase.el")
  :hook ((sql-mode sql-interactive-mode) . sql-upcase-mode))

(provide 'init-sql)
;;; init-sql.el ends here
