;;; maj-sh.el --- Shell Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the shell major mode.
;;
;;; Code:

(use-package sh-script
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
  :hook (;; (bash-ts-mode . lsp-deferred) ; very slow...
         ((bash-ts-mode sh-mode) . flycheck-mode)))

(use-package sh-script
  :if (eq system-type 'gnu/linux)
  :ensure-system-package (shellcheck . "sudo apt install -y shellcheck"))

(use-package shfmt
  ;; Reformat shell script.
  :bind ( :map bash-ts-mode-map
          ("C-c b" . shfmt-buffer) )
  :custom ((shfmt-arguments '("-i" "2")))
  :hook ((bash-ts-mode . shfmt-on-save-mode))
  :ensure-system-package (shfmt . "go install mvdan.cc/sh/v3/cmd/shfmt@latest"))

(use-package bash-completion
  ;; Programmable Bash completion for shell mode.
  :after (corfu cape)
  :config
  (defun bash-completion--capf-setup ()
    ;; TODO(2026-06-16): This doesn't consistently activate bash completion for
    ;; unknown reasons. Revisit.
    (add-hook 'completion-at-point-functions
              #'bash-completion-capf-nonexclusive
              -94 t))
  :hook (bash-ts-mode . bash-completion--capf-setup))

;;; BATS

(use-package bats-mode
  :interpreter "bats")

(provide 'maj-sh)
;;; maj-sh.el ends here
