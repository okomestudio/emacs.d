;;; 80-elfeed.el --- Elfeed  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize Elfeed.
;;
;;; Code:

(use-package elfeed
  :defer t
  :commands (elfeed)

  :custom
  (elfeed-curl-max-connections 16)
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 16)
  (elfeed-search-trailing-width 30)
  (elfeed-show-unique-buffers nil)

  :hook
  (elfeed-show-mode . (lambda ()
                        ;; remove underline from zenkaku space
                        (setq-local nobreak-char-display nil)

                        ;; adjust entry style via shr:
                        (setq-local shr-folding-mode nil
                                    shr-indentation 5
                                    shr-max-image-proportion 0.8
                                    shr-max-width 95
                                    shr-use-fonts t
                                    shr-width nil)))
  (elfeed-search-update . (lambda ()
                            (setq-local nobreak-char-display nil)))

  :config
  (set-face-attribute 'elfeed-search-title-face nil :foreground "#555")
  (set-face-attribute 'elfeed-search-unread-title-face nil :foreground "#000"))


(use-package elfeed-goodies
  :disabled
  :custom
  (elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-goodies/entry-pane-size 0.8)
  (elfeed-goodies/feed-source-column-width 28)
  (elfeed-goodies/tag-column-width 16)

  :config
  (elfeed-goodies/setup)

  ;; Keymap hack to prevent elfeed-goodies from stealing M-v. See
  ;; https://emacs.stackexchange.com/a/65138/599.
  (defvar dummy-elfeed-goodies--map (make-keymap))
  (define-minor-mode dummy-elfeed-goodies--mode
    "Dummy minor mode for keymap hack."
    :init-value t
    :global t
    :keymap dummy-elfeed-goodies--map)
  (add-to-list 'emulation-mode-map-alists `((dummy-elfeed-goodies--mode) . ,dummy-elfeed-goodies--map))
  (define-key dummy-elfeed-goodies--map (kbd "M-v") 'scroll-down-command))

;;; 80-elfeed.el ends here
