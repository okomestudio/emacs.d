;;; maj-python.el --- Python Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Python major mode.
;;
;;; Code:

(use-package python
  :config
  (defun python-ok-format-code ()
    (interactive)
    (blacken-buffer)
    (py-isort-buffer)))

(use-package blacken
  :ensure-system-package
  (black . "pip install black")
  :preface
  (put 'blacken-line-length 'safe-local-variable #'integerp))

(use-package py-isort
  :straight (py-isort
             :host github
             :repo "paetzke/py-isort.el"
             ;; For https://github.com/paetzke/py-isort.el/pull/21
             :fork (:host github
                          :repo "okomestudio/py-isort.el"
                          :branch "ts/provide-default-settings-path"))
  :ensure-system-package
  (isort . "pip install isort"))

(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :custom (pyenv-show-active-python-in-modeline t)
  :hook ((after-init . (lambda () (global-pyenv-mode)))
         (projectile-after-switch-project
          . (lambda (&rest args)
              (pyenv-use-corresponding)
              (shell-command-to-string (locate-user-emacs-file
                                        "bin/bootstrap-python-venv"))))))

(use-package pymacs
  :straight (pymacs
             :host github
             :repo "Pymacs2/Pymacs"
             :post-build  ; see what install-pymacs.sh does:
             (("pip" "install" "-U" "pyopenssl")
              `("pip" "install" "-e" ,(locate-user-emacs-file
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
              `("pip" "install" "-e" ,(locate-user-emacs-file
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
