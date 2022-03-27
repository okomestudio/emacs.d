;;; init-python-elpy.el --- Python via elpy  -*- lexical-binding: t -*-
;;; Commentary:
;;; elpy-based environment (currently disabled)
;;;
;;; Code:

(use-package elpy
  :defer t
  :disabled
  :hook (elpy-mode . ts/elpy-hooks)

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

  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setenv "WORKON_HOME" "~/.pyenv/versions/"))

;; On first install, the following needs to be run within Emacs:
;;
;;   M-x jedi:install-server RET
(use-package jedi-core
  :disabled
  :hook ((elpy-mode) . jedi:setup)
  :config
  (setq jedi:complete-on-dot nil
        jedi:get-in-function-call-delay 500
        jedi:tooltip-method nil  ; or '(pos-tip)
        jedi:use-shortcuts t))

(provide 'init-python-elpy)
;;; init-python-elpy.el ends here
