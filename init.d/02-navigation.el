;;; 02-navigation.el --- Navigation  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure various navigation utilities.
;;
;;; Code:

;;; SEARCH AND MOVEMENT

(use-package ace-isearch
  ;; A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
  :custom
  (ace-isearch-input-length 1)
  (ace-isearch-jump-delay 0.75)
  (ace-isearch-function-from-isearch 'ace-isearch-consult-line-from-isearch)

  :hook
  (after-init . (lambda () (global-ace-isearch-mode +1))))


(use-package ace-jump-mode)


;; WINDOWS AND FRAMES

(use-package ace-window
  :bind
  ("M-O" . 'ace-window)

  :custom (aw-dispatch-always t))


(use-package frame-cmds
  :bind
  ("M-o" . 'other-window-or-frame))


(use-package winner
  ;; Undo or redo a change in window configuration.
  :straight nil
  :bind
  ("C-c <right>" . winner-redo)
  ("C-c <left>". winner-undo)

  :hook
  (after-init . (lambda () (winner-mode 1))))


;; FILES

(use-package recentf
  :straight nil
  :custom
  (recentf-exclude '("/\\.config/emacs/var/"))
  (recentf-mode t))


(use-package save-place
  :straight nil
  :init (save-place-mode 1))


;; IMENU

(use-package imenu-list)

;;; 02-navigation.el ends here
