;;; 65-javascript.el --- javascript  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure js-mode and related utilities.
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
  (setq js-indent-level 2)              ; Setting this in :custom
                                        ; doens't get respected.
  )


(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier")
  :config
  (setq prettier-js-args '("--arrow-parens" "always"
                           "--print-width" "88"
                           "--single-quote"
                           "--trailing-comma" "all")))

;;; 65-javascript.el ends here
