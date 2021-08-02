;;; init-python.el --- Python  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cython-mode
  :after python)

(use-package elpy
  :disabled t

  :config
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
  (defun ts/elpy-hooks ()
    (hs-minor-mode))
  :custom
  (elpy-folding-fringe-indicator t)
  (elpy-rpc-backend "jedi")
  (elpy-rpc-virtualenv-path (or (getenv "VIRTUAL_ENV")
                                "~/.pyenv/versions/3.8.2"
                                (concat user-emacs-directory "elpy/rpc-venv")))
  :defer t
  :hook (elpy-mode . ts/elpy-hooks)
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setenv "WORKON_HOME" "~/.pyenv/versions/"))

;; if using multiple virtual env, this might become useful:
;;
;;   http://stackoverflow.com/questions/21246218/how-can-i-make-emacs-jedi-use-project-specific-virtualenvs
;; (setq jedi:server-args (list (or (buffer-file-name) default-directory)))
;; (push "--sys-path" jedi:server-args)
;; (message "for jedi:server-args %s" jedi:server-args)

;; black -- The opinionated Python code formatter
;;
;; To activate blacken-mode per project basis, place
;;
;;   ((python-mode . ((eval . (blacken-mode 1)))))
;;
;; in .dir-locals.el.
(use-package blacken
  :after python

  :bind
  (:map python-mode-map
   ("C-M-b" . ts/blacken-buffer))

  :ensure-system-package
  (black . "pip install black")

  :if (not (version< emacs-version "25.2"))

  :init
  (defun ts/blacken-buffer ()
    (interactive)
    (blacken-buffer)
    (py-isort-buffer)))

;; jedi.el -- Autocompletion for python
;;
;; On first install, the following needs to be run within Emacs:
;;
;;   M-x jedi:install-server RET
(use-package jedi-core
  :disabled t
  :config
  (setq jedi:complete-on-dot nil
        jedi:get-in-function-call-delay 500
        jedi:tooltip-method nil  ; or '(pos-tip)
        jedi:use-shortcuts t)
  :hook ((elpy-mode) . jedi:setup))

(use-package py-isort
  :after python
  :ensure nil                           ; Use patched version till PR #21 gets merged
  :init
  (ensure-file-from-github
   "okomestudio/py-isort.el/ts/provide-default-settings-path/py-isort.el")
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package pyenv-mode
  :disabled t
  :config
  (pyenv-mode 1))

(use-package python
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i"))
  :custom
  ((python-indent-guess-indent-offset-verbose nil)
   (python-indent-offset 4)))

(use-package python-pytest
  :after (direnv)
  :bind
  (:map python-mode-map
   ("C-c t" . python-pytest-dispatch)))







(provide 'init-python)
;;; init-python.el ends here
