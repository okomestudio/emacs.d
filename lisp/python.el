;;; python.el --- Python  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Python mode configuration.
;;
;;; Code:

(use-package python
  :straight nil
  :custom ((python-indent-guess-indent-offset-verbose nil)
           (python-indent-offset 4)
           (python-shell-interpreter (locate-user-emacs-file
                                      "bin/python-shell-interpreter"))
           ;; lsp
           (lsp-diagnostics-provider :flycheck)
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

           ;; ruff-lsp
           (lsp-ruff-lsp-log-level 'debug)
           (lsp-ruff-lsp-show-notifications 'always))
  :ensure-system-package
  (ipython . "pip install ipython")

  :config
  (add-hook 'python-base-mode-hook #'lsp-deferred 90)
  (add-hook 'python-sql-base-mode-hook #'lsp-deferred 90))

(use-package python-sql-mode
  :straight (:host github :repo "okomestudio/python-sql-mode.el")
  :commands (python-sql-mode python-sql-ts-mode))

;; LINTING, FORMATTING, etc.

(use-package pyimport
  :straight (pyimport
             :host github
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

(use-package ruff-format
  :bind (:map python-mode-map
              ("C-c b" . ruff-format-buffer)
              :map python-ts-mode-map
              ("C-c b" . ruff-format-buffer))
  :hook (((python-mode
           python-ts-mode) . ruff-format-on-save-mode)))

;; VIRTUAL ENVS

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
            (setenv "VIRTUAL_ENV" (pyenv-mode-full-path (pyenv-mode-version)))
            (setenv "PYENV_VIRTUAL_ENV" (pyenv-mode-full-path (pyenv-mode-version)))
            (setq lsp-ruff-lsp-ruff-path (file-name-concat virtual-env
                                                           "bin/ruff"))
            (message (shell-command-to-string
                      (format "%s %s"
                              (locate-user-emacs-file
                               "bin/bootstrap-python-venv")
                              virtual-env)))))))))

;; TESTING

(use-package python-pytest
  :after (direnv)
  :bind (:map python-mode-map
              ("C-c t" . python-pytest-dispatch)))

;; HELP & DOCS

(use-package pydoc
  :bind (;
         :map python-mode-map
         ("C-h o" . pydoc-at-point)
         :map python-ts-mode-map
         ("C-h o" . pydoc-at-point))
  :hook ((python-mode . (lambda () (require 'pydoc)))
         (python-ts-mode . (lambda () (require 'pydoc)))))

;;; python.el ends here
