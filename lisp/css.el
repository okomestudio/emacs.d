;;; css.el --- CSS  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The CSS mode configuration.
;;
;;; Code:

(use-package css-mode
  :straight nil
  :bind (:map
         css-ts-mode-map
         ("C-c b" . prettier-js))
  :custom (css-indent-offset 2)
  :mode (("\\.css\\'" . css-ts-mode))
  :hook (css-ts-mode . lsp))

;;; css.el ends here
