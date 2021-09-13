;;; init-lsp.el --- LSP  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands lsp

  :custom
  ;; TODO: Start the service on start.
  ;;
  ;; Run: docker container run --name explainshell --restart always \
  ;;          -p 5023:5000 -d spaceinvaderone/explainshell
  (lsp-bash-explainshell-endpoint "http://localhost:5023")

  (lsp-bash-highlight-parsing-errors t)
  (lsp-pylsp-configuration-sources ["flake8"])
  ;; (lsp-pylsp-disable-warning t)
  (lsp-pylsp-plugins-flake8-enabled t)
  ;; (lsp-pylsp-plugins-flake8-max-line-length 150)
  ;; (lsp-pylsp-plugins-jedi-use-pyenv-environment t)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-add-ignore '("D100" "D103"))
  (lsp-pylsp-plugins-pydocstyle-convention "google")
  (lsp-pylsp-plugins-pydocstyle-enabled t)
  (lsp-pylsp-server-command '("~/.config/emacs/bin/pylsp"))

  :ensure-system-package
  ((sqls . "go get github.com/lighttiger2505/sqls")
   (unified-language-server . "sudo npm i -g unified-language-server"))

  :hook
  ((dockerfile-mode . (lambda () (ts/lsp-mode-hook 'dockerfile-ls)))
   ;; (html-mode . lsp)
   (js-mode . (lambda () (ts/lsp-mode-hook 'jsts-ls)))
   (json-mode . (lambda () (ts/lsp-mode-hook 'json-ls)))
   (lsp-mode . lsp-enable-which-key-integration)
   (markdown-mode . lsp)
   (python-mode . (lambda () (ts/lsp-mode-hook 'pylsp)))
   (sh-mode . (lambda () (ts/lsp-mode-hook 'bash-ls)))
   (sql-mode . lsp)
   (web-mode . (lambda () (ts/lsp-mode-hook 'html-ls)))
   (yaml-mode . (lambda () (ts/lsp-mode-hook 'yamlls))))

  :init
  (defun ts/lsp-mode-hook (server)
    (lsp-ensure-server server)
    (lsp)) )

;; https://github.com/emacs-grammarly/lsp-grammarly
(use-package lsp-grammarly
  :after (keytar)
  :disabled t

  :custom
  (lsp-grammarly-auto-activate nil)

  :ensure-system-package
  (unofficial-grammarly-language-server . "sudo npm i -g @emacs-grammarly/unofficial-grammarly-language-server")

  :hook
  (text-mode . (lambda () (require 'lsp-grammarly) (lsp)))

  :init
  (use-package keytar
    :ensure-system-package
    (keytar . "sudo npm install -g @emacs-grammarly/keytar-cli")))

(use-package lsp-ui
  :commands lsp-ui-mode

  :custom
  ((lsp-ui-doc-delay 0.5)
   (lsp-ui-doc-position 'at-point)
   (lsp-ui-doc-use-webkit nil)) )

(use-package lsp-treemacs
  :bind ([f7] . lsp-treemacs-symbols)
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
