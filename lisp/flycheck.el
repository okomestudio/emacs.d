;;; flycheck.el --- flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Flycheck configuration.
;;
;;; Code:

(use-package flycheck
  :custom ((flycheck-python-mypy-executable (locate-user-emacs-file "bin/mypy"))
           (flycheck-rst-executable (locate-user-emacs-file "bin/rst2pseudoxml")))
  :hook (((emacs-lisp-mode lisp-data-mode) . flycheck-mode)
         (org-mode . flycheck-mode))
  :preface
  (put 'flycheck-textlint-config 'safe-local-variable #'stringp))

(use-package flycheck
  :if (eq system-type 'gnu/linux)
  :ensure-system-package
  (docutils . "pip install docutils")
  (textlint . "~/.config/emacs/bin/prepare-textlint"))

(use-package flycheck-aspell
  :after (flycheck)
  :hook (org-mode . (lambda () (require 'flycheck-aspell))))

(use-package flycheck-aspell-org
  :straight (:host github :repo "okomestudio/flycheck-aspell-org")
  :after (flycheck-aspell)
  :demand t)

(use-package flycheck-pos-tip
  :custom (flycheck-pos-tip-timeout 60)
  :config (flycheck-pos-tip-mode))

;;; flycheck.el ends here
