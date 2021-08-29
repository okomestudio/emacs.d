;;; init-lsp.el --- LSP  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands lsp

  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "~/.pyenv/shims/pylsp")
  ;;                   :major-modes '(python-mode)
  ;;                   :remote? t
  ;;                   :server-id 'pylsp-remote))

  :custom
  (lsp-pylsp-configuration-sources ["flake8"])
  ;; (lsp-pylsp-disable-warning t)
  (lsp-pylsp-plugins-flake8-enabled t)
  ;; (lsp-pylsp-plugins-flake8-max-line-length 150)
  ;; (lsp-pylsp-plugins-jedi-use-pyenv-environment t)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-add-ignore '("D100" "D103"))
  (lsp-pylsp-plugins-pydocstyle-convention "google")
  (lsp-pylsp-plugins-pydocstyle-enabled t)

  :ensure-system-package
  (
   (lsp-pylsp-server-command . "$(pyenv which pip3) install python-lsp-server[all] python-lsp-black pyls-isort")
   (sqls . "go get github.com/lighttiger2505/sqls")
   (unified-language-server . "sudo npm i -g unified-language-server"))

  :hook
  ((dockerfile-mode . (lambda () (ts/lsp-mode-hook 'dockerfile-ls)))
   ;; (html-mode . lsp)
   (js-mode . (lambda () (ts/lsp-mode-hook 'jsts-ls)))
   (json-mode . (lambda () (ts/lsp-mode-hook 'json-ls)))
   (markdown-mode . lsp)
   (python-mode . lsp)
   (sh-mode . (lambda () (ts/lsp-mode-hook 'bash-ls)))
   (sql-mode . lsp)
   (web-mode . (lambda () (ts/lsp-mode-hook 'html-ls)))
   (yaml-mode . (lambda () (ts/lsp-mode-hook 'yamlls))))

  :init
  (defun ts/lsp-mode-hook (server)
    (lsp-ensure-server server)
    (lsp))

  (defun ts/pyenv-abspath (command)
    (let ((version (car (split-string (shell-command-to-string "pyenv global")))))
      (concat "~/.pyenv/versions/" version "/bin/" command)))

  (setq lsp-pylsp-server-command (ts/pyenv-abspath "pylsp")))

(use-package lsp-ui
  :custom
  ((lsp-ui-doc-delay 0.5)
   (lsp-ui-doc-position 'at-point)
   (lsp-ui-doc-use-webkit nil))

  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :bind ([f7] . lsp-treemacs-symbols)
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
