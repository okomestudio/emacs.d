;;; maj-ibuffer.el --- Ibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Ibuffer, an advanced replacement for the `buffer-menu'.
;;
;;; Code:

(require 'ok)

(use-package ibuffer
  :bind ( :map ibuffer-mode-map
          ("K" . ibuffer-ok--kill-all-but-special) )
  :init (global-set-key [remap list-buffers] 'ibuffer)
  :config
  (defun ibuffer-ok--kill-all-but-special ()
    (interactive)
    (ok-buffer-kill-all-but-special (ibuffer-current-buffer))
    (ibuffer-update nil t))

  ;; In `ibuffer-mode-map', `M-o' binds to `ibuffer-visit-buffer-1-window' by
  ;; default. To avoid the conflict with the default global key binding, unset:
  (define-key ibuffer-mode-map (kbd "M-o") nil))

(use-package nerd-icons-ibuffer
  :custom ((nerd-icons-ibuffer-icon t)
           (nerd-icons-ibuffer-icon-size 1.0))
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'maj-ibuffer)
;;; maj-ibuffer.el ends here
