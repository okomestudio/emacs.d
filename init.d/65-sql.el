;;; 65-sql.el --- sql  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure sql-mode and related utilities.
;;
;;; Code:

(use-package sql
  :bind (nil
         :map sql-mode-map
         ("C-c b" . sql-format-code))
  :hook ((sql-mode . lsp-deferred)      ; Uses `sqls`
         (sql-interactive-mode . (lambda () (setq-local truncate-lines t))))

  :ensure-system-package
  (sqls . "go install github.com/lighttiger2505/sqls@latest")

  :custom
  (lsp-sqls-timeout 30)
  (sql-product 'ansi)

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
    ;; TODO: Replace to-be-obsoleted `defadvice' with `advice-add`.
    (defadvice org-edit-special (before org-edit-src-code activate)
      "Intercept org-src-mode to set SQL product variant."
      (sql-set-product
       (pcase (nth 0 (org-babel-get-src-block-info))
         ("sql" (pcase (cdr (assoc :engine
                                   (nth 2 (org-babel-get-src-block-info))))
                  ("mysql" 'mysql)
                  ("postgres" 'postgres)
                  ("postgresql" 'postgres)
                  (_ 'ansi)))
         ("sqlite" 'sqlite)
         (_ 'ansi))))))


(use-package sqlformat
  ;; Use `pgformatter'.
  :disabled
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-f2" "-g" "-s4" "-U2"
                    "-M" "-p" "\n[ ]*-- sqlfmt: off\n(?:.*)?-- sqlfmt: on\n"))
  :ensure-system-package (pg_format . "sudo apt install -y pgformatter"))


(use-package sqlformat
  ;; Use `sqlfluff'.
  :disabled
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
  :disabled
  :custom (sqlformat-command 'sqlformat)
  :ensure-system-package ("sqlformat" . "pip install sqlparse"))


(use-package sqlformat
  ;; Use `sql-formatter' (github.com/sql-formatter-org/sql-formatter)
  :autoload (sqlformat-args-set)
  :custom (sqlformat-command 'sql-formatter)
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


(use-package sql-upcase
  :disabled
  :straight nil
  :hook ((sql-mode sql-interactive-mode) . sql-upcase-mode)
  :init
  (let* ((dest (expand-file-name "init.d/sql-upcase.el" user-emacs-directory))
         (src-host "https://raw.githubusercontent.com")
         (src-path "emacsmirror/emacswiki.org/master/sql-upcase.el")
         (src-url (file-name-concat src-host src-path)))
    (unless (file-exists-p dest)
      (url-copy-file src-url dest))))

;; Local Variables:
;; nameless-aliases: (("" . "sql"))
;; End:
;;; 65-sql.el ends here
