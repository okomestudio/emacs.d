;;; 55-rst.el --- rst-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rst-mode
  :straight nil

  :ensure-system-package
  (sphinx-quickstart . "pip install sphinx")

  :mode
  "\\.rst\\'")


(use-package lsp-mode
  :hook
  (rst-mode . lsp-deferred)

  :config
  (add-to-list 'lsp-language-id-configuration '(rst-mode . "rst"))
  (lsp-register-client
   (make-lsp-client :new-connection
		                (lsp-stdio-connection '("~/.config/emacs/bin/esbonio"))
                    :activation-fn (lsp-activate-on "rst")
                    :server-id 'esbonio)))

;;; 55-rst.el ends here
