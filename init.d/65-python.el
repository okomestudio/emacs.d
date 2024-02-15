;;; 65-python.el --- Python  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Python and related utilities.
;;
;;; Code:

(use-package python
  :straight nil
  :bind
  (;; no globals
   :map python-mode-map
   ("C-c b" . ok-python--format-python-code)
   :map python-ts-mode-map
   ("C-c b" . ok-python--format-python-code))

  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  (python-shell-interpreter (expand-file-name "bin/python-shell-interpreter"
                                              user-emacs-directory))

  :ensure-system-package (ipython . "pip install ipython")

  :config
  (defun ok-python--format-python-code ()
    "Format Python code."
    (interactive)
    (blacken-buffer)
    (py-isort-buffer)))


(use-package python-sql-mode
  :commands (python-sql-mode python-sql-ts-mode)
  :straight (:host github :repo "okomestudio/python-sql-mode.el"))


(use-package blacken
  :ensure-system-package (black . "pip install black")
  :preface (put 'blacken-line-length 'safe-local-variable #'integerp))


(use-package cython-mode)


(use-package py-isort
  :straight
  (py-isort
   :host github :repo "paetzke/py-isort.el"
   ;; For https://github.com/paetzke/py-isort.el/pull/21
   :fork (:host github
                :repo "okomestudio/py-isort.el"
                :branch "ts/provide-default-settings-path"))

  :ensure-system-package (isort . "pip install isort"))


(use-package pyimport
  :straight
  (:host github :repo "okomestudio/pyimport" :branch "project-awareness"
         :files ("*.el" ("bin/make-imports.py" . "bin/make-imports.py")))

  :ensure-system-package (pyflakes . "pip install pyflakes"))


(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :custom
  (pyenv-show-active-python-in-modeline t)

  :hook
  ;; (after-init . (lambda () (global-pyenv-mode)))
  (projectile-after-switch-project . (lambda (&rest args)
                                       (pyenv-use-corresponding)
                                       (shell-command-to-string
                                        (expand-file-name "bin/bootstrap-python-venv"
                                                          user-emacs-directory)))))


(use-package python-pytest
  :after (direnv)
  :bind
  (;; no globals
   :map python-mode-map
   ("C-c t" . python-pytest-dispatch)))


(use-package devdocs
  :hook
  (python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (python-ts-mode . (lambda () (setq-local devdocs-current-docs '("python~3.12")))))


(use-package pydoc
  :bind
  (;; no globals
   :map python-mode-map
   ("C-h D" . ok-python--pydoc-or-devdocs)
   :map python-ts-mode-map
   ("C-h D" . ok-python--pydoc-or-devdocs))

  :hook
  (python-mode . (lambda () (require 'pydoc)))
  (python-ts-mode . (lambda () (require 'pydoc)))

  :config
  (defun ok-python--pydoc-or-devdocs ()
    (interactive)
    (if (symbol-at-point)
        (pydoc-at-point)
      (devdocs-lookup))))


(use-package lsp-mode
  :custom
  (lsp-pylsp-configuration-sources ["flake8"])
  (lsp-pylsp-plugins-flake8-enabled t)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-enabled nil) ;; redundant with flake8?
  (lsp-pylsp-plugins-pyflakes-enabled nil)
  (lsp-pylsp-plugins-pylint-enabled nil)

  :hook
  (python-mode . lsp-deferred)
  (python-ts-mode . lsp-deferred))


(use-package pymacs
  :disabled
  :straight
  (;;
   :host github :repo "Pymacs2/Pymacs"
   :post-build ;; See what install-pymacs.sh does:
   (("pip" "install" "-U" "pyopenssl")
    `("pip" "install" "-e" ,(file-name-concat user-emacs-directory
                                              "straight/repos/Pymacs/"))))

  :ensure-system-package
  ("/usr/share/doc/python3-dev" . "sudo apt install -y python3-dev")
  ("/usr/include/openssl/ssl.h" . "sudo apt install -y libssl-dev")
  ("/usr/share/doc/libffi-dev" . "sudo apt install -y libffi-dev"))


(use-package ropemacs
  :disabled
  :after (pymacs)
  :straight
  (;;
   :files nil
   :post-build
   (("pip" "install" "-U" "rope")
    `("pip" "install" "-e" ,(file-name-concat user-emacs-directory
                                              "straight/repos/ropemacs/"))))

  :bind
  (("C-x p l" . (lambda ()
                  (interactive)
                  (require 'pymacs)
                  (pymacs-load "ropemacs" "rope-"))))

  :init
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-a")
  (setq ropemacs-autoimport-modules '("os" "shutil" "typing")))

;; Local Variables:
;; nameless-aliases: (("" . "ok-python"))
;; End:
;;; 65-python.el ends here
