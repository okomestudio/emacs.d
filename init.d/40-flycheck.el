;;; 40-flycheck.el --- flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure flycheck related utilities.
;;
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

  :hook
  (emacs-lisp-mode . flycheck-mode))


(use-package flycheck-pos-tip
  :custom (flycheck-pos-tip-timeout 60)
  :config (flycheck-pos-tip-mode))

;;; 40-flycheck.el ends here
