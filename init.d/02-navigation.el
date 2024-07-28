;;; 02-navigation.el --- Navigation  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure various navigation utilities.
;;
;;; Code:

;;; SEARCH AND MOVEMENT

(use-package ace-isearch
  ;; A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
  :bind (;
         :map isearch-mode-map
         ("C-'" . 'ace-isearch-jump-during-isearch))

  :custom
  (ace-isearch-input-length 6)
  (ace-isearch-jump-delay 0.3)
  (ace-isearch-function 'avy-goto-char)
  (ace-isearch-function-from-isearch #'ace-isearch-consult-line-from-isearch)

  :hook (after-init . (lambda () (global-ace-isearch-mode +1))))

(use-package avy)


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


;; WINDOW SCROLLING

(use-package mwheel
  :straight nil
  :custom
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)
                                 ((meta))
                                 ((control meta) . global-text-scale)
                                 ((control) . text-scale))))


(use-package pixel-scroll
  :straight nil
  :hook (after-init . (lambda () (pixel-scroll-precision-mode +1)))
  :custom
  (pixel-scroll-precision-interpolation-factor 1.2)
  (pixel-scroll-precision-large-scroll-height 1.0)
  (pixel-scroll-precision-momentum-min-velocity 0.5)
  (pixel-scroll-precision-momentum-seconds 0.5) ;; 1.75
  (pixel-scroll-precision-momentum-tick 0.05)
  (pixel-scroll-precision-interpolation-total-time 0.1)
  (pixel-scroll-precision-interpolation-between-scroll 0.001))


(use-package yascroll
  :hook (after-init . (lambda () (global-yascroll-bar-mode 1))))


;; FILES

(use-package recentf
  :straight nil
  :bind ("C-x r F" . recentf)
  :custom (recentf-mode t)
  :config (push "/\\.config/emacs/var/" recentf-exclude))


(use-package save-place
  :straight nil
  :init (save-place-mode 1))


;; IMENU

(use-package imenu-list)


;; BOOKMARK

(use-package bookmark+
  ;; Enhances vanilla Emacs bookmarks in many way.
  :defer 5)

;;; 02-navigation.el ends here
