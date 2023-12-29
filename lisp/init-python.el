;;; init-python.el --- Python  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package python
  :after (blacken py-isort)

  :bind
  (:map python-mode-map
   ("C-c b" . init-python--format-python-code)
   :map python-ts-mode-map
   ("C-c b" . init-python--format-python-code))

  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  (python-shell-interpreter "~/.pyenv/shims/ipython")
  (python-shell-interpreter-args (concat "-i --simple-prompt "
                                         "--InteractiveShell.display_page=True"))

  :ensure-system-package
  (ipython . "pip install ipython")

  :preface
  (defun init-python--format-python-code ()
    "Format Python code."
    (interactive)
    (blacken-buffer)
    (py-isort-buffer))

  :init
  (add-to-list 'treesit-language-source-alist
               '(python "https://github.com/tree-sitter/tree-sitter-python.git"))

  ;; Run treesit-install-language-grammar before using.
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))


(use-package polymode
  :mode
  ("\\.py[iw]?\\'" . pm-python-sql-mode)

  :interpreter
  (("python" . pm-python-sql-mode)
   ("python3" . pm-python-sql-mode))

  :config
  (define-hostmode init-python--pm-python-hostmode
    :mode 'python-mode)

  (define-hostmode init-python--pm-python-ts-hostmode init-python--pm-python-hostmode
    :mode 'python-ts-mode)

  (define-innermode init-python--pm-sql-expr-python-innermode
    :mode 'sql-mode
    :head-matcher "\"\\{3\\}--[[:blank:]]*\\(sql\\|SQL\\)"
    :tail-matcher "\"\\{3\\}"
    :head-mode 'host
    :tail-mode 'host)

  (defun init-python--pm-python-sql-eval-chunk (beg end msg)
    "Calls out to `sql-send-region' with the polymode chunk region"
    (sql-send-region beg end))

  (define-polymode pm-python-sql-mode
    :hostmode 'init-python--pm-python-ts-hostmode
    :innermodes '(init-python--pm-sql-expr-python-innermode)

    (setq polymode-eval-region-function #'init-python--pm-python-sql-eval-chunk)
    (define-key pm-python-sql-mode-map
                (kbd "C-c C-c") 'polymode-eval-chunk)))


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

  :config
  (add-hook 'switch-buffer-functions
            (lambda (_prev curr)
              (if (string-equal "Python"
                                (format-mode-line mode-name nil nil curr))
                  (pyenv-use-corresponding))))

  (global-pyenv-mode))


(use-package python-pytest
  :after (direnv)

  :bind
  (:map python-mode-map
        ("C-c t" . python-pytest-dispatch)))


(provide 'init-python)
;;; init-python.el ends here
