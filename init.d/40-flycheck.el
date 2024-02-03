;;; 40-flycheck.el --- flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :defer t

  :custom
  (flycheck-python-mypy-executable "~/.config/emacs/bin/mypy")
  (flycheck-rst-executable "~/.config/emacs/bin/rst2pseudoxml")

  :ensure-system-package
  (docutils . "pip install docutils")
  (textlint . "~/.config/emacs/bin/prepare-textlint")

  :preface
  (put 'flycheck-textlint-config 'safe-local-variable #'stringp)

  :hook
  (after-init . global-flycheck-mode))


(use-package flycheck-pos-tip
  :defer t

  :custom
  (flycheck-pos-tip-timeout 60)

  :config
  (flycheck-pos-tip-mode))

;;; 40-flycheck.el ends here
