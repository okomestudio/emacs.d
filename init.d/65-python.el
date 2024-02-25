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
   ("C-c b" . ok-python--format-python-code-with-ruff)
   :map python-ts-mode-map
   ("C-c b" . ok-python--format-python-code-with-ruff))

  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  (python-shell-interpreter (expand-file-name "bin/python-shell-interpreter"
                                              user-emacs-directory))

  ;; lsp
  (lsp-diagnostics-provider :flycheck)
  (lsp-lens-enable t)
  (lsp-ui-doc-delay 2)
  (lsp-ui-doc-enable t)

  ;; pylsp
  (lsp-pylsp-configuration-sources ["flake8"])
  (lsp-pylsp-plugins-flake8-enabled t)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-enabled nil)
  (lsp-pylsp-plugins-pyflakes-enabled nil)
  (lsp-pylsp-plugins-pylint-enabled nil)

  ;; ruff-lsp
  (lsp-ruff-lsp-log-level 'debug)
  (lsp-ruff-lsp-show-notifications 'always)

  :ensure-system-package (ipython . "pip install ipython")

  :config
  (defun ok-python--format-python-code-with-ruff ()
    (interactive)
    (let ((this-file (or (buffer-file-name)
                         (buffer-name)))
          (output-buffer "*ruff*")
          (error-buffer "*ruff-error*")
          (command-format "ruff format --stdin-filename %s")
          (command-lint "ruff check --fix --select \"I\" --stdin-filename %s")
          (orig-content (buffer-string))
          exit-code)
      (save-excursion
        (setq exit-code
              (shell-command-on-region (point-min)
                                       (point-max)
                                       (format command-format this-file)
                                       output-buffer
                                       t
                                       error-buffer))
        (if (> exit-code 0)
            (progn
              (replace-region-contents (point-min)
                                       (point-max)
                                       (lambda () orig-content))
              (display-buffer error-buffer #'display-buffer-pop-up-window))
          (setq exit-code
                (shell-command-on-region (point-min)
                                         (point-max)
                                         (format command-lint this-file)
                                         output-buffer
                                         t
                                         error-buffer))
          (when (> exit-code 0)
            (replace-region-contents (point-min)
                                     (point-max)
                                     (lambda () orig-content))
            (display-buffer error-buffer #'display-buffer-pop-up-window))))))

  (defun ok-python-format-python-code-with-black-and-isort ()
    (interactive)
    (blacken-buffer)
    (py-isort-buffer))

  (add-hook 'python-base-mode-hook #'lsp-deferred 90)
  (add-hook 'python-sql-base-mode-hook #'lsp-deferred 90))


(use-package python-sql-mode
  :commands (python-sql-mode python-sql-ts-mode)
  :straight (:host github :repo "okomestudio/python-sql-mode.el"))


;; LINTING, FORMATTING, etc.

(use-package blacken
  :disabled ;; use Ruff
  :ensure-system-package (black . "pip install black")
  :preface (put 'blacken-line-length 'safe-local-variable #'integerp))


(use-package py-isort
  :disabled ;; use Ruff
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
  (:host github :repo "Wilfred/pyimport" :branch "master"
         :fork (:host github :repo "okomestudio/pyimport" :branch "venv-support")
         :files ("*.el" ("bin/make-imports.py" . "bin/make-imports.py")))
  :ensure-system-package (pyflakes . "pip install pyflakes")
  :preface (put 'pyimport-develop-packages 'safe-local-variable #'listp))


;; VIRTUAL ENVS

(use-package pyenv
  :disabled
  :straight (:host github :repo "aiguofer/pyenv.el")
  :custom (pyenv-show-active-python-in-modeline t)
  :hook
  (after-init . (lambda () (global-pyenv-mode)))
  (projectile-after-switch-project . (lambda (&rest args)
                                       (pyenv-use-corresponding)
                                       (shell-command-to-string
                                        (expand-file-name "bin/bootstrap-python-venv"
                                                          user-emacs-directory)))))


(use-package pyenv-mode
  :straight (:host github :repo "pythonic-emacs/pyenv-mode")
  :hook ((python-base-mode python-sql-base-mode) . ok-projectile-pyenv-mode-set)
  :config
  (defun ok-projectile-pyenv-mode-set ()
    (unless (featurep 'pyenv-mode)
      (pyenv-mode 1))
    (let* ((project-root-dir (projectile-project-root))
           (python-version-file (expand-file-name ".python-version" project-root-dir)))
      (if (not (file-exists-p python-version-file))
          (pyenv-mode-unset)
        (let ((python-version (s-trim (with-temp-buffer
                                        (insert-file-contents python-version-file)
                                        (buffer-string)))))
          (pyenv-mode-set python-version)
          (let ((virtual-env (pyenv-mode-full-path (pyenv-mode-version))))
            (setenv "VIRTUAL_ENV" (pyenv-mode-full-path (pyenv-mode-version)))
            (setenv "PYENV_VIRTUAL_ENV" (pyenv-mode-full-path (pyenv-mode-version)))
            (setq lsp-ruff-lsp-ruff-path (file-name-concat virtual-env "bin/ruff"))
            (message (shell-command-to-string
                      (format "%s %s"
                              (expand-file-name "bin/bootstrap-python-venv"
                                                user-emacs-directory)
                              virtual-env)))))))))


;; TESTING

(use-package python-pytest
  :after (direnv)
  :bind
  (;; no globals
   :map python-mode-map
   ("C-c t" . python-pytest-dispatch)))


;; HELP & DOCS

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


;; PYMACS

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

;; MISC.

(use-package cython-mode :disabled)

;; Local Variables:
;; nameless-aliases: (("" . "ok-python"))
;; End:
;;; 65-python.el ends here
