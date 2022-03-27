;;; init-python.el --- Python  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :after (blacken py-isort)
  :bind (:map python-mode-map ("C-c b" . ts/beautify-python))

  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  (python-shell-interpreter "~/.pyenv/versions/3.9.7/bin/ipython")
  (python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

  :config
  (defun ts/beautify-python ()
    (interactive)
    (blacken-buffer)
    (py-isort-buffer)))

(use-package blacken
  :ensure-system-package (black . "pip install black")
  :init (put 'blacken-line-length 'safe-local-variable #'integerp))

(use-package cython-mode)

;; https://github.com/paetzke/py-isort.el
(use-package py-isort
  :ensure nil                           ; Use patched version till PR #21 gets merged
  :init
  (ensure-file-from-github
   "okomestudio/py-isort.el/ts/provide-default-settings-path/py-isort.el"))

(use-package pyenv
  :ensure nil
  :hook (switch-buffer-functions . ts/pyenv-update-on-buffer-switch)

  :config
  (defun ts/pyenv-update-on-buffer-switch (prev curr)
    (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
        (pyenv-use-corresponding)))

  (require 'pyenv)
  (setq pyenv-show-active-python-in-modeline nil)
  (setq pyenv-use-alias nil)
  (setq pyenv-set-path nil)
  (global-pyenv-mode)

  :init
  (ensure-file-from-github "aiguofer/pyenv.el/master/pyenv.el")

  ;; This hook runs just after switching buffer.
  ;; github.com/10sr/switch-buffer-functions-el
  (use-package switch-buffer-functions))

(use-package python-pytest
  :after (direnv)
  :bind (:map python-mode-map ("C-c t" . python-pytest-dispatch)))

(provide 'init-python)
;;; init-python.el ends here
