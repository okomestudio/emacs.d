;;; init-rest.el --- Rest  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package restclient
  :after (jq-mode)
  :init
  (ensure-file-from-github "pashky/restclient.el/master/restclient-jq.el"))

(provide 'init-rest)
;;; init-rest.el ends here
