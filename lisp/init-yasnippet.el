;;; init-yasnippet.el --- Yasnippet  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :config
  (yas-reload-all)

  :hook
  ((prog-mode . yas-minor-mode)))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
