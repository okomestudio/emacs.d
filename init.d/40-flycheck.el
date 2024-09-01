;;; 40-flycheck.el --- flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure flycheck related utilities.
;;
;;; Code:

(use-package flycheck
  :custom
  (flycheck-python-mypy-executable (expand-file-name "bin/mypy"
                                                     user-emacs-directory))
  (flycheck-rst-executable (expand-file-name "bin/rst2pseudoxml"
                                             user-emacs-directory))

  :hook
  ((emacs-lisp-mode lisp-data-mode) . flycheck-mode)
  (org-mode . flycheck-mode)

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

;;; 40-flycheck.el ends here
