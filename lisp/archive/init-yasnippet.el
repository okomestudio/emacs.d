;;; init-yasnippet.el --- Yasnippet  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
