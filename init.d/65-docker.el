;;; 65-docker.el --- Docker  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Docker related utilities.
;;
;;; Code:

(use-package docker)

(use-package dockerfile-mode
  :hook
  (dockerfile-mode . (lambda ()
                       (setq-local devdocs-current-docs '("docker"))))
  (dockerfile-mode . (lambda ()
                       (lsp-ensure-server 'dockerfile-ls)
                       (lsp-deferred))))

;;; 65-docker.el ends here
