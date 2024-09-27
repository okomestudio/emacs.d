;;; sql.el --- SQL  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; SQL mode configuration.
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
  :bind (:map sql-mode-map
              ("C-c b" . sql-format-code))
  :hook ((sql-mode . lsp-deferred)      ; uses `sqls'
         (sql-interactive-mode . (lambda () (setq-local truncate-lines t))))
  :custom ((lsp-sqls-timeout 30)
           (sql-product 'ansi))
  :ensure-system-package
  (sqls . "go install github.com/lighttiger2505/sqls@latest")

  :preface
  (ok-safe-local-variable-add sql-connection-alist listp
                              sql-postgres-login-params listp
                              sql-postgres-options listp
                              sql-postgres-program stringp)

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
       ('mysql '("-l" "mysql"))
       ('postgres
        `("-l" "postgresql"
          "-c" ,(concat "{\"keywordCase\": \"upper\", "
                        "\"dataTypeCase\": \"upper\", "
                        "\"functionCase\": \"upper\", "
                        "\"paramTypes\": {\"custom\": ["
                        "{\"regex\": \"%\\\\([a-zA-Z0-9_]+\\\\)s\"}" ; Python string
                        "]}}")))
       ('sqlite '("-l" "sqlite"))
       (_ nil)))))

;;; sql.el ends here
