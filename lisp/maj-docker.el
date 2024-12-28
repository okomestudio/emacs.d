;;; maj-docker.el --- Docker Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Docker major mode.
;;
;;; Code:

(use-package docker)

(use-package dockerfile-ts-mode
  :mode "Dockerfile\\'"
  :hook (dockerfile-ts-mode . (lambda ()
                                (lsp-ensure-server 'dockerfile-ls)
                                (lsp-deferred))))

(provide 'maj-docker)
;;; maj-docker.el ends here
