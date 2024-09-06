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

(use-package elfeed-goodies
  :disabled
  :custom ((elfeed-goodies/entry-pane-position 'bottom)
           (elfeed-goodies/entry-pane-size 0.8)
           (elfeed-goodies/feed-source-column-width 28)
           (elfeed-goodies/tag-column-width 16))
  :config
  (elfeed-goodies/setup)

  ;; Keymap hack to prevent elfeed-goodies from stealing M-v. See
  ;; https://emacs.stackexchange.com/a/65138/599.
  (defvar dummy-elfeed-goodies--map (make-keymap))
  (define-minor-mode dummy-elfeed-goodies--modex
    "Dummy minor mode for keymap hack."
    :init-value t
    :global t
    :keymap dummy-elfeed-goodies--map)
  (add-to-list 'emulation-mode-map-alists `((dummy-elfeed-goodies--mode) . ,dummy-elfeed-goodies--map))
  (define-key dummy-elfeed-goodies--map (kbd "M-v") 'scroll-down-command))

;;; elfeed.el ends here
