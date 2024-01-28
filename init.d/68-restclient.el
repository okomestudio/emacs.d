;;; 68-restclient.el --- restclient.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package restclient ;; derives from fundamental-mode
  ;; HTTP REST client.
  :defer t)

(use-package company-restclient
  :defer t)

(use-package ob-restclient
  ;; An org-mode extension to restclient.el
  :defer t)

;;; 68-restclient.el ends here
