;;; init-ido.el --- Ido  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ido
  :disabled t                           ; disabled in favor of fido
  :ensure nil

  :config
  (setq ido-enable-flex-matching t)
  ;; (setq ido-everywhere t)

  (add-to-list 'ido-ignore-files "\\.egg-info/$")
  (add-to-list 'ido-ignore-files "^\\.eggs/$")
  (add-to-list 'ido-ignore-files "^\\.pytest_cache/$")
  (add-to-list 'ido-ignore-files "^__pycache__/$")
  (add-to-list 'ido-ignore-files "^build/$")
  (add-to-list 'ido-ignore-files "^dist/$")
  ;; Note that ido-ignore-directories does not appear to affect C-x C-f
  ;; (add-to-list 'ido-ignore-directories "src")

  (ido-mode 1))

(provide 'init-ido)
;;; init-ido.el ends here
