;;; rst.el --- RST  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The RST mode configuration.
;;
;; See https://docutils.sourceforge.io/docs/user/emacs.html
;;
;;; Code:

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(rst-mode . "rst"))
  (lsp-register-client
   (make-lsp-client
    :new-connection
		(lsp-stdio-connection `(,(locate-user-emacs-file "bin/esbonio")))
    :activation-fn (lsp-activate-on "rst")
    :server-id 'esbonio)))

(use-package poly-rst
  :hook (rst-mode . lsp-deferred))

;;; rst.el ends here
