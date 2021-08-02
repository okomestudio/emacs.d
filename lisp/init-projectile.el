;;; init-projectile.el --- Projectile  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide 'init-projectile)
;;; init-projectile.el ends here
