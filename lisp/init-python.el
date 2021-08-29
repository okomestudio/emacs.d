;;; init-python.el --- Python  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :after (blacken py-isort)

  :bind
  (:map python-mode-map ("C-c b" . ts/beautify-python))

  :custom
  ((python-indent-guess-indent-offset-verbose nil)
   (python-indent-offset 4))

  :config
  (defun ts/beautify-python ()
    (interactive)
    (blacken-buffer)
    (py-isort-buffer)))

(use-package blacken
  :if (not (version< emacs-version "25.2"))

  :ensure-system-package
  (black . "pip install black")

  :init
  (put 'blacken-line-length 'safe-local-variable #'integerp))

(use-package cython-mode
  :after python)

(use-package py-isort
  :ensure nil                           ; Use patched version till PR #21 gets merged

  :init
  (ensure-file-from-github
   "okomestudio/py-isort.el/ts/provide-default-settings-path/py-isort.el"))

(use-package pyenv-mode
  :disabled t

  :config
  (pyenv-mode 1))

(use-package python-pytest
  :after (direnv)

  :bind
  (:map python-mode-map ("C-c t" . python-pytest-dispatch)))

(provide 'init-python)
;;; init-python.el ends here
