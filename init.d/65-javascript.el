;;; 65-javascript.el --- javascript  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure JavaScript related utilities.
;;
;;; Code:

(use-package js-ts-mode
  :straight nil
  :mode "\\.js\\'"
  :hook
  (js-ts-mode . (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (js-ts-mode . lsp))


(use-package js-jsx-mode
  :straight nil
  :mode "\\.jsx\\'"
  :hook
  (js-jsx-mode . (lambda () (setq-local devdocs-current-docs '("javascript"
                                                               "axios"
                                                               "react"))))
  (js-jsx-mode . lsp))


(use-package typescript-ts-mode
  :straight nil
  :mode ("\\.tsx?\\'")
  :hook
  (typescript-ts-mode . (lambda ()
                          (setq-local devdocs-current-docs '("javascript"
                                                             "axios"
                                                             "typescript"
                                                             "vite"))))
  (typescript-ts-mode . lsp))

;;; 65-javascript.el ends here
