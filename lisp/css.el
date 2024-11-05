;;; css.el --- CSS  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The CSS mode configuration.
;;
;;; Code:

(use-package css-ts-mode
  :straight nil
  :bind (:map
         css-ts-mode-map
         ("C-c b" . (lambda () (interactive) (prettier-js))))
  :custom (css-indent-offset 2)
  :hook (css-ts-mode . lsp))

;;; css.el ends here
