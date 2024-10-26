;;; shell.el --- shell  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the shell mode and related utilities.
;;
;;; Code:

(use-package sh-script
  :straight nil
  :interpreter (("bash" . bash-ts-mode)
                ("sh" . sh-mode))
  :mode (("\\.?bash_.*\\'" . bash-ts-mode)
         ("\\.?bashrc\\'" . bash-ts-mode)
         (".*\\.sh\\'" . sh-mode))
  :hook ((bash-ts-mode . lsp-deferred)
         (sh-mode . flymake-mode))
  :custom
  (sh-basic-offset 2)
  ;; Run the Explainshell service on start:
  ;;
  ;;   docker container run --name explainshell --restart always \
  ;;          -p 5023:5000 -d spaceinvaderone/explainshell
  ;;
  (lsp-bash-explainshell-endpoint "http://localhost:5023")
  (lsp-bash-highlight-parsing-errors t))


(use-package sh-script
  :if (eq system-type 'gnu/linux)
  :straight nil
  :ensure-system-package (shellcheck . "sudo apt install -y shellcheck"))


(use-package bats-mode
  :interpreter "bats")

;;; shell.el ends here
