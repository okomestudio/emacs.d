;;; init-restclient.el --- restclient.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package restclient
  :after (jq-mode)
  :init
  (ensure-file-from-github "pashky/restclient.el/master/restclient-jq.el"))

(provide 'init-restclient)
;;; init-restclient.el ends here
