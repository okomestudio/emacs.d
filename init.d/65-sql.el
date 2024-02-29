;;; 65-sql.el --- SQL  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure SQL and related utilities.
;;
;;; Code:

(use-package sql
  :bind (nil
         :map sql-mode-map
         ("C-c b" . ok-sqlformat-format-code))
  :hook (sql-interactive-mode . (lambda () (setq-local truncate-lines t)))
  :custom (sql-product 'ansi)
  :preface
  (put 'sql-connection-alist 'safe-local-variable #'listp)
  (put 'sql-postgres-login-params 'safe-local-variable #'listp)
  (put 'sql-postgres-options 'safe-local-variable #'listp)
  (put 'sql-postgres-program 'safe-local-variable #'stringp)

  :config
  (require 'sqlformat)

  (defun ok-sqlformat-format-code (beg end)
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (save-excursion
      (sqlformat beg end)
      (delete-trailing-whitespace)))

  (with-eval-after-load 'org
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
  ;; The sqlfluff version of sqlformat.
  :hook (sql-mode . ok-set-sqlformat-args)
  :custom (sqlformat-command 'sqlfluff)
  :ensure-system-package ("sqlfluff" . "pip install sqlfluff")
  :config
  (defun ok-set-sqlformat-args ()
    "Set formatter dialect based on sql-dialect."
    (setq-local sqlformat-args
                (pcase sql-dialect
                  ('mysql '("-vvv" "--dialect" "mysql"))
                  ('postgres '("-vvv" "--dialect" "postgres"))
                  ('sqlite '("-vvv" "--dialect" "sqlite" "-e" "CP02"))
                  (_ '("-vvv" "--dialect" "ansi"))))))


(use-package sqlformat
  ;; The pgformatter version of sqlformat.
  :disabled
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args
   '("-f2" "-g" "-s4" "-U2"
     "-M" "-p" "\n[ ]*-- sqlfmt: off\n(?:.*)?-- sqlfmt: on\n"))

  :ensure-system-package
  (pg_format . "sudo apt install -y pgformatter"))


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


(use-package lsp-mode
  :custom (lsp-sqls-timeout 30)
  ;; :hook (sql-mode . lsp-deferred)       ; Uses `sqls'
  :ensure-system-package
  (sqls . "go install github.com/lighttiger2505/sqls@latest"))

;; Local Variables:
;; nameless-aliases: (("" . "prefix"))
;; End:
;;; 65-sql.el ends here
