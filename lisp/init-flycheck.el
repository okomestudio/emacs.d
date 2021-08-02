;;; init-flycheck.el --- Flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package flycheck-pos-tip
  :init
  (setq flycheck-pos-tip-timeout 60)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package flymake-shellcheck
  :disabled t
  :ensure-system-package (shellcheck . "sudo apt install shellcheck")
  :if (executable-find "shellcheck")
  :commands flymake-shellcheck-load
  :hook ((sh-mode) . flymake-shellcheck-load)
  :init
  (setq sh-basic-offset 4
        sh-indentation 4)
  (add-to-list 'auto-mode-alist '("\\.bats\\'" . sh-mode))
  (add-to-list 'interpreter-mode-alist '("bats" . sh-mode)))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (shell-script-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here