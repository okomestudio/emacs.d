;;; init-python.el --- Python  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :after (blacken py-isort)

  :bind
  (:map python-mode-map
        ("C-c b" . (lambda ()
                     (interactive)
                     (blacken-buffer)
                     (py-isort-buffer))))

  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  (python-shell-interpreter "~/.pyenv/shims/ipython")
  (python-shell-interpreter-args (concat "-i --simple-prompt "
                                         "--InteractiveShell.display_page=True"))

  :ensure-system-package
  (ipython . "pip install ipython"))


(use-package blacken
  :ensure-system-package
  (black . "pip install black")

  :init
  (put 'blacken-line-length 'safe-local-variable #'integerp))


(use-package cython-mode)


(use-package py-isort
  :straight
  (py-isort
   :type git :host github :repo "paetzke/py-isort.el"

   ;; Use patch till PR #21 gets merged
   :fork (:host github
                :repo "okomestudio/py-isort.el"
                :branch "ts/provide-default-settings-path"))

  :ensure-system-package
  (isort . "pip install isort"))


(use-package pyenv
  :after switch-buffer-functions

  :straight
  (:host github :repo "aiguofer/pyenv.el")

  :custom
  (pyenv-show-active-python-in-modeline nil)

  :hook
  (switch-buffer-functions
   .
   (lambda (_prev curr)
     (interactive)
     (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
         (pyenv-use-corresponding))))

  :config
  (global-pyenv-mode))


(use-package python-pytest
  :after (direnv)

  :bind
  (:map python-mode-map
        ("C-c t" . python-pytest-dispatch)))

(provide 'init-python)
;;; init-python.el ends here
