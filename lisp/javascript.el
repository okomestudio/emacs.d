;;; javascript.el --- JavaScript  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The JavaScript mode configuration.
;;
;;; Code:

(use-package js
  :straight nil
  :bind (:map
         js-ts-mode-map
         ("C-c b" . (lambda () (interactive) (prettier-js))))
  :custom (js-jsx-indent-level 2)
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)  ; equivalent to `js-jsx-mode'
         ("\\.tsx?\\'" . typescript-ts-mode))
  :hook ((js-ts-mode
          typescript-ts-mode) . js-ok--init)
  :config
  ;; Setting the following custom variable in :custom does not get
  ;; respected...
  (setopt js-indent-level 2)

  (defun js-ok--init ()
    (setq-local fill-column 90)
    (lsp-deferred)))

(use-package prettier-js
  :ensure-system-package (prettier . "npm install -g prettier"))

;;; javascript.el ends here
