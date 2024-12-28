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
           (python-shell-interpreter
            (locate-user-emacs-file "bin/python-shell-interpreter")))
  :hook (((python-mode python-ts-mode) . ruff-isort-format-on-save-mode)
         ((python-mode python-ts-mode) . ruff-format-on-save-mode))
  :ensure-system-package (ipython . "pip install ipython")
  :config
  ;;; Ruff formatter (github.com/scop/emacs-ruff-format is not yet mature)
  (require 'reformatter)
  (reformatter-define ruff-format
    :program "ruff"
    :args (list "format" "-v" "--stdin-filename"
                (or (buffer-file-name) input-file))
    :lighter " RuffFmt"
    :group 'ruff-format)

  ;; isort
  (reformatter-define ruff-isort-format
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

(use-package pyenv-mode
  :straight (:host github :repo "pythonic-emacs/pyenv-mode")
  :hook ((python-base-mode
          python-sql-base-mode) . pyenv-mode-ok-projectile-set)
  :config
  (defun pyenv-mode-ok-projectile-set ()
    (unless (featurep 'pyenv-mode)
      (pyenv-mode 1))
    (let* ((project-root-dir (projectile-project-root))
           (python-version-file (expand-file-name ".python-version"
                                                  project-root-dir)))
      (if (not (file-exists-p python-version-file))
          (pyenv-mode-unset)
        (let ((python-version (s-trim
                               (with-temp-buffer
                                 (insert-file-contents python-version-file)
                                 (buffer-string)))))
          (pyenv-mode-set python-version)
          (let ((virtual-env (pyenv-mode-full-path (pyenv-mode-version))))
            (setenv "VIRTUAL_ENV" virtual-env)
            (setenv "PYENV_VIRTUAL_ENV" virtual-env)))))))

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
    (message (shell-command-to-string
              (locate-user-emacs-file "bin/bootstrap-pylsp")))
    (message (shell-command-to-string
              (locate-user-emacs-file "bin/bootstrap-ruff")))
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
           (lsp-pyright-langserver-command "pyright"))
  :init
  (defun lsp-pyright-ok--start ()
    (message (shell-command-to-string
              (locate-user-emacs-file "bin/bootstrap-pyright")))
    (require 'lsp-pyright)
    (setq-local lsp-disabled-clients '(pylsp ruff))
    (lsp-deferred))
  (add-hook 'python-base-mode-hook #'lsp-pyright-ok--start 90)
  (add-hook 'python-sql-base-mode-hook #'lsp-pyright-ok--start 90))

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
  (pyflakes . "pip install pyflakes")
  :preface
  (put 'pyimport-develop-packages 'safe-local-variable #'listp))

;; TODO(2024-12-15): Try for docstring formatting:
;;
;;   - github.com/DanielNoord/pydocstringformatter
;;   - github.com/PyCQA/docformatter

;; TESTING

(use-package python-pytest
  :bind (:map python-mode-map
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
