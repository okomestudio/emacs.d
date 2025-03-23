;;; maj-python.el --- Python Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Python major mode.
;;
;;; Code:

;;; LSP

(use-package lsp-mode
  ;; Set up `lsp-pylsp' for use.
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
           (lsp-pylsp-plugins-ruff-enabled t))
  :config
  (defun lsp-pylsp-ok--start ()
    (message (shell-command-to-string (ok-file-expand-bin "bootstrap-pylsp")))
    (message (shell-command-to-string (ok-file-expand-bin "bootstrap-ruff")))
    (setq-local lsp-disabled-clients '(lsp-pyright))
    (lsp-deferred)))

;;; Linting, formatting, etc.

(use-package python
  :config
  (defun python-ok-format-code ()
    (interactive)
    (blacken-buffer)
    (py-isort-buffer)))

(use-package blacken
  :ensure-system-package (black . "pip install black"))

(use-package py-isort
  :straight (py-isort
             :host github
             :repo "paetzke/py-isort.el"
             ;; For https://github.com/paetzke/py-isort.el/pull/21
             :fork ( :host github
                     :repo "okomestudio/py-isort.el"
                     :branch "ts/provide-default-settings-path" ))
  :ensure-system-package (isort . "pip install isort"))

(use-package pyimport
  :straight
  (pyimport :host github
            :repo "Wilfred/pyimport"
            :branch "master"
            :fork ( :host github
                    :repo "okomestudio/pyimport"
                    :branch "venv-support" )
            :files ("*.el" ("bin/make-imports.py" . "bin/make-imports.py")))
  :ensure-system-package (pyflakes . "pip install pyflakes"))

;;; Virtual env management

(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :custom (pyenv-show-active-python-in-modeline t)
  :hook ((after-init . global-pyenv-mode)
         (projectile-after-switch-project
          . (lambda (&rest args)
              (pyenv-use-corresponding)
              (shell-command-to-string (ok-file-expand-bin "bootstrap-python-venv"))))))

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

;;; Pymacs

(use-package pymacs
  :straight (pymacs
             :host github
             :repo "Pymacs2/Pymacs"
             :post-build            ; see what install-pymacs.sh does:
             (("pip" "install" "-U" "pyopenssl")
              `("pip" "install" "-e" ,(ok-file-expand-user-emacs-file
                                       "straight/repos/Pymacs/"))))
  :ensure-system-package
  ("/usr/share/doc/python3-dev" . "sudo apt install -y python3-dev")
  ("/usr/include/openssl/ssl.h" . "sudo apt install -y libssl-dev")
  ("/usr/share/doc/libffi-dev" . "sudo apt install -y libffi-dev"))

(use-package ropemacs
  :after (pymacs)
  :straight (ropemacs
             :files nil
             :post-build
             (("pip" "install" "-U" "rope")
              `("pip" "install" "-e" ,(ok-file-expand-user-emacs-file
                                       "straight/repos/ropemacs/"))))
  :bind (("C-x p l" . (lambda ()
                        (interactive)
                        (require 'pymacs)
                        (pymacs-load "ropemacs" "rope-"))))
  :init
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-a")
  (setq ropemacs-autoimport-modules '("os" "shutil" "typing")))

(use-package cython-mode)

(provide 'maj-python)
;;; maj-python.el ends here
