;;; maj-sql.el --- SQL Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the SQL major mode.
;;
;; USAGE:
;;
;; In a buffer in which `sql-mode' is active, run
;; `sql-set-sqli-buffer' interactively to connect to an existing
;; database.
;;
;; Define known connections in `sql-connection-alist' in
;; .dir-locals.el for convenience.
;;
;;; Code:

(use-package sql
  :bind ( :map sql-mode-map
          ("C-c b" . sql-format-code)
          :map sql-interactive-mode-map
          ("M-n" .	comint-next-input)
          ("M-p"	.	comint-previous-input)
          ("M-r"	. comint-history-isearch-backward-regexp) )
  :custom ((lsp-sqls-timeout 30)
           (sql-product 'ansi))
  :hook ((sql-mode . lsp-deferred) ; uses `sqls'
         (sql-interactive-mode . (lambda () (setq-local truncate-lines t))))
  :ensure-system-package
  (sqls . "go install github.com/lighttiger2505/sqls@latest")
  :config
  (defun sql-format-code (beg end)
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (save-excursion
      (sqlformat-args-set)
      (sqlformat beg end)
      (delete-trailing-whitespace)))

  (with-eval-after-load 'org
    (defun org-edit-special-ok--set-sql-product (&rest r)
      "Intercept org-src-mode to set SQL product variant."
      (let ((lang (nth 0 (org-babel-get-src-block-info))))
        (when (member lang '("sql" "sqlite"))
          (sql-set-product
           (pcase lang
             ("sql" (pcase (cdr (assoc :engine
                                       (nth 2 (org-babel-get-src-block-info))))
                      ("mysql" 'mysql)
                      ("postgres" 'postgres)
                      ("postgresql" 'postgres)
                      (_ 'ansi)))
             ("sqlite" 'sqlite)
             (_ 'ansi))))))

    (advice-add #'org-edit-special
                :before #'org-edit-special-ok--set-sql-product)))

(use-package sqlformat
  ;; Use `sql-formatter' (github.com/sql-formatter-org/sql-formatter)
  :custom (sqlformat-command 'sql-formatter)
  :autoload (sqlformat-args-set)
  :ensure-system-package (sql-formatter . "npm install -g sql-formatter")
  :config
  (defun sqlformat-args-set ()
    "Set formatter variant from dialect."
    (setq-local
     sqlformat-args
     (pcase sql-dialect
       ('ansi
        `("-l" "sql"
          "-c" ,(json-serialize
                 '((keywordCase . "upper")
                   (dataTypeCase . "upper")
                   (functionCase . "upper")))))
       ('mysql '("-l" "mysql"))
       ('postgres
        `("-l" "postgresql"
          "-c" ,(json-serialize
                 '((keywordCase . "upper")
                   (dataTypeCase . "upper")
                   (functionCase . "upper")
                   (paramTypes . (custom [(regex "%\\([a-zA-Z0-9_]+\\)s")]))))))
       ('sqlite
        `("-l" "sqlite"
          "-c" ,(json-serialize
                 '((keywordCase . "upper")
                   (dataTypeCase . "upper")
                   (functionCase . "upper")
                   (paramTypes . (custom [(regex "\\?")]))))))
       (_ nil)))))

(provide 'maj-sql)
;;; maj-sql.el ends here
