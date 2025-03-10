;;; maj-python.el --- Python Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Python major mode.
;;
;;; Code:

(use-package python
  :straight nil
  :bind (:map
         python-mode-map
         ("C-c b" . python-ok-format-buffer)
         :map python-ts-mode-map
         ("C-c b" . python-ok-format-buffer))
  :custom ((python-indent-guess-indent-offset-verbose nil)
           (python-indent-offset 4)
           (python-shell-interpreter (ok-file-expand-bin "python-shell-interpreter")))
  :ensure-system-package (ipython . "pip install ipython")
  :config
  ;; Code formatting with Ruff.
  (require 'reformatter)
  (reformatter-define ruff-format
    ;; Provides `ruff-format-buffer' and `ruff-format-on-save-mode'.
    :program "ruff"
    :args (list "format" "-v" "--stdin-filename"
                (or (buffer-file-name) input-file))
    :lighter " RuffFmt"
    :group 'ruff-format)

  (reformatter-define ruff-isort-format
    ;; Provides `ruff-isort-format-buffer' and `ruff-isort-format-on-save-mode'.
    :program "ruff"
    :args (list "check" "--select" "I" "--fix" "--stdin-filename"
                (or (buffer-file-name) input-file))
    :lighter " RuffIsort"
    :group 'ruff-format)

  (defun python-ok-format-buffer ()
    (interactive)
    (ruff-isort-format-buffer)
    (ruff-format-buffer)))

(use-package python-sql-mode
  :straight (:host github :repo "okomestudio/python-sql-mode.el")
  :commands (python-sql-mode python-sql-ts-mode))

;;; VIRTUAL ENVS

(use-package pet
  :ensure-system-package
  ((dasel . "go install github.com/tomwright/dasel/v2/cmd/dasel@master")
   ;; (tomljson . "go install github.com/pelletier/go-toml/v2/cmd/tomljson@latest")
   ;; (yq . "sudo apt install -y yq")
   (sqlite3 . "sudo apt install -y sqlite3"))
  :init
  (defun pet-ok--init ()
    (require 'pet)

    ;; Unless we clear and set venv path here, the environment
    ;; variable may be pointing to the path picked up when Emacs
    ;; launched.
    (setenv "VIRTUAL_ENV" nil) ; (file-name-directory (pet-python-version-path))

    (setq-local python-shell-virtualenv-root (pet-virtualenv-root))
    (when python-shell-virtualenv-root
      (setq-local python-shell-interpreter (pet-executable-find "python"))

      ;; Flycheck
      ;; (pet-flycheck-setup)
      ;; (flycheck-mode)

      ;; Pytest
      (setq-local python-pytest-executable (pet-executable-find "pytest"))

      ;; Ruff
      (when-let ((ruff-executable (pet-executable-find "ruff")))
        (setq-local ruff-format-command ruff-executable)
        (ruff-isort-format-on-save-mode)
        (ruff-format-on-save-mode))

      ;; lsp-pyright
      (require 'lsp-pyright)
      (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                  lsp-pyright-venv-path python-shell-virtualenv-root)
      (lsp-pyright-ok--start)))
  (add-hook 'python-base-mode-hook #'pet-ok--init -10)
  (add-hook 'python-sql-base-mode-hook #'pet-ok--init -10))

;;; LSP

(use-package lsp-mode
  ;; Enable for lsp-pylsp.
  :disabled
  :custom ((lsp-diagnostics-provider :flycheck)
           (lsp-lens-enable t)
           (lsp-ui-doc-delay 2)
           (lsp-ui-doc-enable t)

           ;; pylsp
           (lsp-pylsp-plugins-flake8-enabled nil)
           (lsp-pylsp-plugins-pycodestyle-enabled nil)
           (lsp-pylsp-plugins-pydocstyle-enabled nil)
           (lsp-pylsp-plugins-pyflakes-enabled nil)
           (lsp-pylsp-plugins-pylint-enabled nil)
           (lsp-pylsp-plugins-ruff-enabled t)

           ;; ruff
           (lsp-ruff-log-level 'debug)
           (lsp-ruff-show-notifications 'always)
           (lsp-ruff-python-path "python3"))
  :config
  (defun lsp-pylsp-ok--start ()
    (message (shell-command-to-string (ok-file-expand-bin "bootstrap-pylsp")))
    (message (shell-command-to-string (ok-file-expand-bin "bootstrap-ruff")))
    (setq-local lsp-disabled-clients '(lsp-pyright))
    (lsp-deferred))
  (add-hook 'python-base-mode-hook #'lsp-pylsp-ok--start 90)
  (add-hook 'python-sql-base-mode-hook #'lsp-pylsp-ok--start 90))

(use-package lsp-pyright
  ;; Enable for lsp-pyright.
  :custom ((lsp-diagnostics-provider :flycheck)
           (lsp-lens-enable t)
           (lsp-ui-doc-delay 2)
           (lsp-ui-doc-enable t)
           (lsp-pyright-auto-import-completions t)
           (lsp-pyright-langserver-command "pyright")
           (lsp-pyright-log-level "debug"))
  :config
  (defun lsp-pyright-ok--start ()
    (message (shell-command-to-string (ok-file-expand-bin "bootstrap-pyright")))
    (setq-local lsp-disabled-clients '(pylsp ruff))
    (lsp-deferred)))

;;; LINTING, FORMATTING, etc.

(use-package pyimport
  :straight (pyimport :host github
                      :repo "Wilfred/pyimport"
                      :branch "master"
                      :fork (:host github
                                   :repo "okomestudio/pyimport"
                                   :branch "venv-support")
                      :files ("*.el" ("bin/make-imports.py" . "bin/make-imports.py")))
  :ensure-system-package
  (pyflakes . "pip install pyflakes"))

;; TODO(2024-12-15): Try for docstring formatting:
;;
;;   - github.com/DanielNoord/pydocstringformatter
;;   - github.com/PyCQA/docformatter

;; TESTING

(use-package python-pytest
  :bind (:map
         python-mode-map
         ("C-c t" . python-pytest-dispatch)
         :map python-ts-mode-map
         ("C-c t" . python-pytest-dispatch))
  :hook ((python-mode
          python-ts-mode) . (lambda () (require 'python-pytest))))

;; HELP & DOCS

(use-package pydoc
  :bind (:map
         python-mode-map
         ("C-h o" . pydoc-at-point)
         :map python-ts-mode-map
         ("C-h o" . pydoc-at-point))
  :hook ((python-mode
          python-ts-mode) . (lambda () (require 'pydoc))))

(provide 'maj-python)
;;; maj-python.el ends here
