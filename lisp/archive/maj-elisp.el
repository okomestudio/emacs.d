;;; maj-elisp.el --- Elisp Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elsa
  :ensure-system-package
  (eask . "curl -fsSL https://raw.githubusercontent.com/emacs-eask/cli/master/webinstall/install.sh | sh")
  :config (elsa-lsp-register))

(use-package flycheck-elsa
  :custom (flycheck-elsa-backend 'eask)
  :hook (emacs-lisp-mode . flycheck-elsa-setup))

(provide 'maj-elisp)
;;; maj-elisp.el ends here
