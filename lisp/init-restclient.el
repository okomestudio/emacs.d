;;; init-restclient.el --- restclient.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package restclient
  ;; HTTP REST client.
  :defer t)

(use-package company-restclient
  :defer t)

(use-package ob-restclient
  ;; An org-mode extension to restclient.el
  :defer t)

(provide 'init-restclient)
;;; init-restclient.el ends here
