;;; init-ido.el --- Ido  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ido
  :disabled t                           ; disabled to use fido
  :ensure nil
  :config
  (setq ido-enable-flex-matching t)
  ;; (setq ido-everywhere t)
  (ido-mode 1)
  (add-to-list 'ido-ignore-files "\\.egg-info/$")
  (add-to-list 'ido-ignore-files "^\\.eggs/$")
  (add-to-list 'ido-ignore-files "^\\.pytest_cache/$")
  (add-to-list 'ido-ignore-files "^__pycache__/$")
  (add-to-list 'ido-ignore-files "^build/$")
  (add-to-list 'ido-ignore-files "^dist/$")
  ;; Note that ido-ignore-directories does not appear to affect C-x C-f
  ;; (add-to-list 'ido-ignore-directories "src")
  )

(provide 'init-ido)
;;; init-ido.el ends here
