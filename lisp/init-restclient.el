;;; init-restclient.el --- restclient.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; HTTP REST client (github.com/pashky/restclient.el)
(use-package restclient
  :defer t)

(use-package company-restclient
  :defer t)

;; An org-mode extension to restclient.el (github.com/alf/ob-restclient.el)
(use-package ob-restclient
  :defer t)

(provide 'init-restclient)
;;; init-restclient.el ends here
