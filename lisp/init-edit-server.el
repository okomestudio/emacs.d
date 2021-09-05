;;; init-edit-server.el --- Edit-Server  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Server that responds to edit requests from Chrome
;; -------------------------------------------------
;; https://github.com/stsquad/emacs_chrome
;; https://www.emacswiki.org/emacs/Edit_with_Emacs
(use-package edit-server
  :init
  (when (daemonp)
    (edit-server-start)))

(provide 'init-edit-server)
;;; init-edit-server.el ends here
