;;; maj-sql.el --- SQL Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the SQL major mode.
;;
;;; Code:

(use-package sqlformat
  ;; Use `pgformatter'.
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-f2" "-g" "-s4" "-U2"
                    "-M" "-p" "\n[ ]*-- sqlfmt: off\n(?:.*)?-- sqlfmt: on\n"))
  :ensure-system-package (pg_format . "sudo apt install -y pgformatter"))

(use-package sqlformat
  ;; Use `sqlfluff'.
  :autoload (sqlformat-args-set)
  :custom (sqlformat-command 'sqlfluff)
  :ensure-system-package ("sqlfluff" . "pip install sqlfluff")
  ;; :hook (sql-mode . ok-set-sqlformat-args)
  :config
  (defun sqlformat-args-set ()
    "Set formatter dialect."
    (setq-local sqlformat-args
                (pcase sql-dialect
                  ('mysql '("-vvv" "--dialect" "mysql"))
                  ('postgres '("-vvv" "--dialect" "postgres"))
                  ('sqlite '("-vvv" "--dialect" "sqlite" "-e" "CP02"))
                  (_ '("-vvv" "--dialect" "ansi"))))))

(use-package sqlformat
  ;; Use `sqlformat'.
  :custom (sqlformat-command 'sqlformat)
  :ensure-system-package ("sqlformat" . "pip install sqlparse"))

(use-package sql-upcase
  :hook ((sql-mode sql-interactive-mode) . sql-upcase-mode)
  ;; :init
  ;; (let* ((dest (expand-file-name "init.d/sql-upcase.el" user-emacs-directory))
  ;;        (src-host "https://raw.githubusercontent.com")
  ;;        (src-path "emacsmirror/emacswiki.org/master/sql-upcase.el")
  ;;        (src-url (file-name-concat src-host src-path)))
  ;;   (unless (file-exists-p dest)
  ;;     (url-copy-file src-url dest)))
  )

(provide 'maj-sql)
;;; maj-sql.el ends here
