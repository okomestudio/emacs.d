;;; 65-sql.el --- SQL  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sql
  :after sqlformat

  :bind
  (;
   :map sql-mode-map
   ("C-c b" . init-sql--beautify-sql))

  :hook
  (sql-interactive-mode . (lambda () (setq truncate-lines t)))
  (sql-mode . (lambda () (init-sql--set-sqlformat-args)))

  :preface
  (defun init-sql--set-sqlformat-args ()
    "Set formatter dialect based on sql-dialect."
    (cond ((string= sql-dialect 'mysql)
           (setq sqlformat-args '("-vvv" "--dialect" "mysql")))
          ((string= sql-dialect 'postgres)
           (setq sqlformat-args '("-vvv" "--dialect" "postgres")))
          ((string= sql-dialect 'sqlite)
           (setq sqlformat-args '("-vvv" "--dialect" "sqlite" "-e" "CP02"
                                  ;; "--show-lint-violations"
                                  ;; "--ignore-local-config"
                                  )))
          (t (setq sqlformat-args '("-vvv" "--dialect" "ansi")))))

  (put 'sql-connection-alist 'safe-local-variable #'listp)
  (put 'sql-postgres-login-params 'safe-local-variable #'listp)
  (put 'sql-postgres-options 'safe-local-variable #'listp)
  (put 'sql-postgres-program 'safe-local-variable #'stringp)

  :config
  (with-eval-after-load 'org
    (defadvice org-edit-special (before org-edit-src-code activate)
      "Intercept org-src-mode to set SQL product variant."
      (let ((lang (nth 0 (org-babel-get-src-block-info))))
        (cond
         ((string= lang "sql")
          (let ((engine (cdr (assoc :engine
                                    (nth 2 (org-babel-get-src-block-info))))))
            (cond ((string= engine "mysql") (sql-set-product 'mysql))
                  ((or (string= engine "postgres")
                       (string= engine "postgresql")) (sql-set-product 'postgres))
                  (t (sql-set-product 'ansi)))))
         ((string= lang "sqlite") (sql-set-product 'sqlite))
         (t (sql-set-product 'ansi))))))

  (sql-set-product 'ansi))


(use-package sqlformat
  ;; The sqlfluff version of sqlformat.
  :custom
  (sqlformat-command 'sqlfluff)

  :ensure-system-package
  ("~/.pyenv/shims/sqlfluff" . "~/.pyenv/shims/pip3 install sqlfluff")

  :preface
  (defun init-sql--beautify-sql (beg end)
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (save-excursion
      (sqlformat beg end)
      (delete-trailing-whitespace))))


(use-package sqlformat
  ;; The pgformatter version of sqlformat.
  :disabled

  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args
   '("-f2" "-g" "-s4" "-U2"
     "-M" "-p" "\n[ ]*-- sqlfmt: off\n(?:.*)?-- sqlfmt: on\n"))

  :ensure-system-package
  (pg_format . "sudo apt install pgformatter"))


(use-package sql-upcase
  :disabled
  :ensure nil

  :hook ((sql-mode sql-interactive-mode) . sql-upcase-mode)

  :init
  (require 'okutil)
  (okutil-ensure-file-from-github
   "emacsmirror/emacswiki.org/master/sql-upcase.el"))


(use-package lsp-mode
  :custom
  ;; (lsp-sqls-workspace-config-path "root")
  (lsp-sqls-timeout 30)

  :hook
  (sql-mode . lsp-deferred) ;; uses 'sqls

  :ensure-system-package
  (sqls . "go install github.com/lighttiger2505/sqls@latest"))

;;; 65-sql.el ends here
