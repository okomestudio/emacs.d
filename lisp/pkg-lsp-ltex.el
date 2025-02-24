;;; pkg-lsp-ltex.el --- lsp-ltex  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up LTeX Language Server client.
;;
;; `lsp-ltex' supports LanguageTool with support for LaTeX.
;;
;;; Code:

(use-package lsp-ltex
  :custom ((lsp-ltex-active-modes '(bibtex-mode
                                    context-mode
                                    gfm-mode
                                    latex-mode
                                    markdown-mode
                                    org-mode
                                    rst-mode
                                    text-mode))
           (lsp-ltex-log-level "finest")
           (lsp-ltex-trace-server "verbose")
           (lsp-ltex-version "16.0.0"))
  :hook ((gfm-mode
          markdown-mode
          ;; let's not use ltex by default; can load manually with `lsp'
          ;; org-mode
          rst-mode) . lsp-ltex-start)
  :commands (lsp-ltex-start lsp-ltex-restart)
  :config
  (defvar lsp-ltex-aspell-dict "~/.aspell.en.pws")

  (defun lsp-ltex--load-dict-from-file (file)
    (vconcat (string-lines (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string)))))

  (defun lsp-ltex--update-dictionary ()
    (setq-local lsp-ltex-dictionary
                `(:en-US ,(lsp-ltex--load-dict-from-file lsp-ltex-aspell-dict)))
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

(provide 'pkg-lsp-ltex)
;;; pkg-lsp-ltex.el ends here
