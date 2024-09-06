;;; javascript.el --- JavaScript  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; JavaScript mode configuration.
;;
;;; Code:

(use-package js
  :straight nil
  :bind (nil
         :map js-ts-mode-map
         ("C-c b" . (lambda () (interactive) (prettier-js))))
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)     ; js-jsx-mode is equivalent
         ("\\.tsx?\\'" . typescript-ts-mode))
  :hook ((js-ts-mode
          typescript-ts-mode) . lsp-deferred)
  :custom
  (js-jsx-indent-level 2)

  :config
  (setopt js-indent-level 2)            ; Setting this in :custom
                                        ; doens't get respected.
  )


(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier"))

;;; javascript.el ends here
