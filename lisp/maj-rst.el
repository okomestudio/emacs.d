;;; maj-rst.el --- ReStructuredText (RST) Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the RST major mode.
;;
;; See https://docutils.sourceforge.io/docs/user/emacs.html
;;
;;; Code:

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(rst-mode . "rst"))
  (lsp-register-client
   (make-lsp-client
    :new-connection
		(lsp-stdio-connection `(,(ok-file-expand-bin "esbonio")))
    :activation-fn (lsp-activate-on "rst")
    :server-id 'esbonio)))

(use-package poly-rst
  :hook ((rst-mode . lsp-deferred)
         (rst-mode . rst-ok--init))
  :config
  (defun rst-ok--init ()
    (setq-local fill-column 90)))

(provide 'maj-rst)
;;; maj-rst.el ends here
