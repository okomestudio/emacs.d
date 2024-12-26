;;; subsys-flycheck.el --- Flycheck Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Flycheck subsystem.
;;
;;; Code:

(use-package flycheck
  :custom ((flycheck-python-mypy-executable (locate-user-emacs-file "bin/mypy"))
           (flycheck-rst-executable (locate-user-emacs-file "bin/rst2pseudoxml")))
  :hook (((emacs-lisp-mode lisp-data-mode) . flycheck-mode)
         (org-mode . flycheck-mode))
  :preface (put 'flycheck-textlint-config 'safe-local-variable #'stringp)
  :config
  (flycheck-define-checker write-good
    "The write-good prose checker."
    :command ("write-good" "--parse" source-inplace)
    :standard-input nil
    :error-patterns ((warning
                      line-start
                      (file-name) ":" line ":" column ":" (message)
                      line-end))
    :modes (gfm-mode markdown-mode org-mode text-mode))
  (add-to-list 'flycheck-checkers 'write-good))

(use-package flycheck
  :if (eq system-type 'gnu/linux)
  :ensure-system-package
  (docutils . "pip install docutils")
  (textlint . "~/.config/emacs/bin/prepare-textlint"))

(use-package flycheck-aspell
  :after (flycheck)
  :hook (org-mode . (lambda () (require 'flycheck-aspell))))

(use-package flycheck-aspell-org
  :straight (:host github :repo "okomestudio/flycheck-aspell-org.el")
  :after (flycheck-aspell)
  :demand t
  :config
  (flycheck-add-next-checker 'org-aspell-dynamic 'write-good))

(use-package flycheck-pos-tip
  :custom (flycheck-pos-tip-timeout 60)
  :config (flycheck-pos-tip-mode))

(provide 'subsys-flycheck)
;;; subsys-flycheck.el ends here
