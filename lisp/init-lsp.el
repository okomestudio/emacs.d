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
  ;(lsp-pylsp-disable-warning t)
  (lsp-pylsp-plugins-flake8-enabled t)
  ;(lsp-pylsp-plugins-flake8-max-line-length 150)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-add-ignore '("D100" "D103"))
  (lsp-pylsp-plugins-pydocstyle-convention "google")
  (lsp-pylsp-plugins-pydocstyle-enabled t)
  (lsp-pylsp-server-command "~/.pyenv/shims/pylsp")

  :ensure-system-package
  ((isort-with-pyenv . "~/.pyenv/versions/$(pyenv global)/bin/pip3 install isort[pyproject]")
   (pylsp-with-pyenv . "~/.pyenv/versions/$(pyenv global)/bin/pip3 install python-lsp-server[all] pyls-black pyls-isort")
   (bash-language-server . "sudo npm i -g bash-language-server")
   (javascript-typescript-langserver . "sudo npm i -g javascript-typescript-langserver")
   (sqls . "go get github.com/lighttiger2505/sqls")
   (unified-language-server . "sudo npm i -g unified-language-server")
   (vscode-json-languageserver . "sudo npm i -g vscode-json-languageserver"))

  :hook
  ((dockerfile-mode . lsp)
   (json-mode . lsp)
   (markdown-mode . lsp)
   (python-mode . lsp)
   (sh-mode . lsp)
   (sql-mode . lsp)
   (yaml-mode . lsp))

  :init
  (defun ts/get-global-pypath (exe)
    (let ((ver (car (split-string (shell-command-to-string "pyenv global")))))
      (concat "~/.pyenv/versions/" ver "/bin/" exe)))
  (setq pylsp-with-pyenv (ts/get-global-pypath "pylsp"))
  (setq isort-with-pyenv (ts/get-global-pypath "isort")))

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
