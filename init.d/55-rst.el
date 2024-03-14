;;; 55-rst.el --- rst  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the RST mode and related utilities.
;;
;;; Code:

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(rst-mode . "rst"))
  (lsp-register-client
   (make-lsp-client
    :new-connection
		(lsp-stdio-connection `(,(expand-file-name "bin/esbonio"
                                               user-emacs-directory)))
    :activation-fn (lsp-activate-on "rst")
    :server-id 'esbonio)))


(use-package poly-rst
  :hook (rst-mode . lsp-deferred))

;;; 55-rst.el ends here
