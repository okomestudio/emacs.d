;;; 65-shell.el --- Shell  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure shell related utilities.
;;
;;; Code:

(use-package sh-script
  :straight nil
  :interpreter "sh"
  :mode ".*\\.sh\\'"
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)

  :hook
  (sh-mode . flymake-mode))


(use-package bash-ts-mode
  :straight nil
  :interpreter "bash"
  :mode ("\\.?bash_.*\\'" "\\.?bashrc\\'")
  :hook (bash-ts-mode . flymake-mode))


(use-package bats-mode
  :interpreter "bats")


(use-package lsp-mode
  ;; Run the Explainshell service on start:
  ;;
  ;;   docker container run --name explainshell --restart always \
  ;;          -p 5023:5000 -d spaceinvaderone/explainshell
  ;;
  :custom
  (lsp-bash-explainshell-endpoint "http://localhost:5023")
  (lsp-bash-highlight-parsing-errors t)

  :hook
  (bash-ts-mode . (lambda ()
                    (lsp-ensure-server 'bash-ls)
                    (lsp-deferred)))

  :ensure-system-package
  (shellcheck . "sudo apt install -y shellcheck"))

;;; 65-shell.el ends here
