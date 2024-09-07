;;; elfeed.el --- elfeed  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Elfeed configuration.
;;
;;; Code:

(use-package elfeed
  :hook ((elfeed-show-mode . elfeed-ok-show-setup)
         (elfeed-search-update . elfeed-ok-search-setup))
  :custom ((elfeed-curl-max-connections 16)
           (elfeed-search-title-max-width 100)
           (elfeed-search-title-min-width 16)
           (elfeed-search-trailing-width 30)
           (elfeed-show-unique-buffers nil))
  :config
  (setq elfeed-log-level 'info)  ; 'info or 'debug

  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground (face-attribute 'shadow :foreground))
  (set-face-attribute 'elfeed-search-unread-title-face nil
                      :foreground (face-attribute 'default :foreground))

  (defun elfeed-ok-show-setup ()
    ;; Remove underline from zenkaku space
    (setq-local nobreak-char-display nil)

    ;; Adjust entry style via shr:
    (setq-local shr-folding-mode nil
                shr-indentation 4
                shr-max-image-proportion 0.8
                shr-use-fonts t
                shr-width 80))

  (defun elfeed-ok-search-setup ()
    (setq-local nobreak-char-display nil)))

;;; elfeed.el ends here
