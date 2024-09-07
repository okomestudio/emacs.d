;;; elfeed-goodies.el --- Elfeed Goodies  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Elfeed Goodies configuration.
;;
;;; Code:

(use-package elfeed-goodies
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

  (add-to-list 'emulation-mode-map-alists
               `((dummy-elfeed-goodies--mode) . ,dummy-elfeed-goodies--map))
  (define-key dummy-elfeed-goodies--map (kbd "M-v") 'scroll-down-command))

;;; elfeed-goodies.el ends here
