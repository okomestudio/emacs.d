;;; subsys-eglot.el --- Eglot  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure eglot.
;;
;;; Code:

(use-package eglot
  :straight (:type built-in)
  :config

  ;; Harper
  ;;
  ;; References:
  ;; - https://www.cyan.sh/blog/posts/goodbye-languagetool-hello-harper.html
  (dolist (mode '(org-mode))
    (add-to-list 'eglot-server-programs `(,mode . ("harper-ls" "--stdio")))))

(provide 'subsys-eglot)
;;; subsys-eglot.el ends here
