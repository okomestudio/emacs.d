;;; init-edit-server.el --- Edit-Server  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Server that responds to edit requests from Chrome
(use-package edit-server
  :if window-system
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(provide 'init-edit-server)
;;; init-edit-server.el ends here
