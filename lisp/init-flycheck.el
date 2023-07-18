;;; init-flycheck.el --- flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package flycheck
  :custom
  (flycheck-python-mypy-executable "~/.config/emacs/bin/mypy")
  (flycheck-rst-executable "~/.config/emacs/bin/rst2pseudoxml")

  :ensure-system-package
  (docutils . "pip install docutils")
  (textlint . "~/.config/emacs/bin/prepare-textlint")

  :preface
  (put 'flycheck-textlint-config 'safe-local-variable #'stringp)

  :init
  (global-flycheck-mode))


(use-package flycheck-pos-tip
  :custom
  (flycheck-pos-tip-timeout 60)

  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
