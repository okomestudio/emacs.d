;;; maj-css.el --- CSS Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the CSS major mode.
;;
;;; Code:

(use-package css-mode
  :bind ( :map css-ts-mode-map
          ("C-c b" . prettier-js) )
  :custom (css-indent-offset 2)
  :mode (("\\.css\\'" . css-ts-mode))
  :hook (css-ts-mode . lsp))

(provide 'maj-css)
;;; maj-css.el ends here
