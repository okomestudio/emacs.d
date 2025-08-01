;;; maj-python.el --- Python Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Python major mode.
;;
;;; Code:

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
  :ensure-system-package (isort . "pip install isort"))

(use-package pyimport
  :ensure-system-package (pyflakes . "pip install pyflakes"))

;;; Virtual env management

(use-package pyenv
  :custom (pyenv-show-active-python-in-modeline t)
  :hook ((after-init . global-pyenv-mode)
         (projectile-after-switch-project
          . (lambda (&rest args)
              (pyenv-use-corresponding)
              (shell-command-to-string (ok-file-expand-bin "bootstrap-python-venv"))))))

(use-package pyenv-mode
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
  :ensure-system-package
  ("/usr/share/doc/python3-dev" . "sudo apt install -y python3-dev")
  ("/usr/include/openssl/ssl.h" . "sudo apt install -y libssl-dev")
  ("/usr/share/doc/libffi-dev" . "sudo apt install -y libffi-dev"))

(use-package ropemacs
  :after (pymacs)
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
