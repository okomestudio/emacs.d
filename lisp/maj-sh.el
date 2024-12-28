;;; maj-sh.el --- Shell Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the shell major mode.
;;
;;; Code:

(use-package sh-script
  :straight nil
  :custom ((sh-basic-offset 2)
           ;; Run the Explainshell service on start:
           ;;
           ;;   docker container run --name explainshell --restart always \
           ;;          -p 5023:5000 -d spaceinvaderone/explainshell
           ;;
           (lsp-bash-explainshell-endpoint "http://localhost:5023")
           (lsp-bash-highlight-parsing-errors t))
  :interpreter (("bash" . bash-ts-mode)
                ("sh" . sh-mode))
  :mode (("\\.?bash_.*\\'" . bash-ts-mode)
         ("\\.?bashrc\\'" . bash-ts-mode)
         (".*\\.sh\\'" . sh-mode))
  :hook ((bash-ts-mode . lsp-deferred)
         (sh-mode . flymake-mode)))

(use-package sh-script
  :straight nil
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (shellcheck . "sudo apt install -y shellcheck"))

(use-package bats-mode
  :interpreter "bats")

(provide 'maj-sh)
;;; maj-sh.el ends here
