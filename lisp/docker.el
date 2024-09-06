;;; docker.el --- Docker  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Docker related utilities.
;;
;;; Code:

(use-package docker)

(use-package dockerfile-ts-mode
  :mode "Dockerfile\\'"
  :hook
  (dockerfile-ts-mode . (lambda ()
                          (lsp-ensure-server 'dockerfile-ls)
                          (lsp-deferred))))

;;; docker.el ends here
