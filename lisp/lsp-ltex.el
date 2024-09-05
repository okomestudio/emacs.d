;;; lsp-ltex.el --- lsp-ltex  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; LTeX Language Server supports LanguageTool with support for LaTeX.
;;
;;; Code:

(use-package lsp-ltex
  :commands (lsp-ltex-start lsp-ltex-restart)
  :hook ((markdown-mode
          ;; org-mode ;; let's not use ltex by default; can load manually with `lsp'
          rst-mode) . lsp-ltex-start)
  :custom
  (lsp-ltex-version "16.0.0")
  (lsp-ltex-log-level "finest")
  (lsp-ltex-trace-server "verbose")

  :preface
  (put 'lsp-ltex-language 'safe-local-variable #'stringp)

  :config
  (defvar lsp-ltex-aspell-dict "~/.aspell.en.pws")

  (defun lsp-ltex--load-dict-from-file (file)
    (vconcat (string-lines (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string)))))

  (defun lsp-ltex--update-dictionary ()
    (setq-local lsp-ltex-dictionary `(:en-US ,(lsp-ltex--load-dict-from-file lsp-ltex-aspell-dict)))
    (lsp-register-custom-settings '(("ltex.dictionary" lsp-ltex-dictionary))))

  (defun lsp-ltex--maybe-recheck (_)
    "Refresh LTEX dictionary of aspell dict update."
    (when (bound-and-true-p lsp-mode)
      (lsp-ltex-restart)))

  (advice-add #'ispell-pdict-save :after #'lsp-ltex--maybe-recheck)

  (defun lsp-ltex-start ()
    "Start LTEX language server."
    (interactive)
    (require 'lsp-ltex)
    (setq-local lsp-disabled-clients '(grammarly-ls)
                lsp-headerline-breadcrumb-enable nil)
    (lsp-ltex--update-dictionary)
    (lsp-deferred))

  (defun lsp-ltex-restart ()
    "Restart LTEX language server after dictionary update."
    (interactive)
    (lsp-ltex--update-dictionary)
    (lsp-workspace-restart (lsp--read-workspace))))

;;; lsp-ltex.el ends here
