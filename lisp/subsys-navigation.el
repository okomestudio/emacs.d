;;; subsys-navigation.el --- Navigation Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the navigation subsystem.
;;
;; NOTE:
;;
;;   - For `isearch-forward' with a different IME, press enter as the
;;     first command after `C-s'.
;;
;;; Code:

;;; SEARCH AND MOVEMENT

(use-package ace-isearch
  ;; A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
  :bind (:map isearch-mode-map
              ("C-'" . ace-isearch-jump-during-isearch))
  :custom ((ace-isearch-input-length 6)
           (ace-isearch-jump-delay 0.3)
           (ace-isearch-function #'avy-goto-char)
           (ace-isearch-function-from-isearch #'ace-isearch-consult-line-from-isearch))
  :hook (on-first-input . (lambda () (global-ace-isearch-mode +1))))

(use-package avy)

;;; WINDOWS AND FRAMES

(use-package ace-window
  :bind ("M-O" . ace-window)
  :custom (aw-dispatch-always t))

(use-package frame-cmds
  :bind ("M-o" . other-window-or-frame))

(use-package winner
  ;; Undo or redo a change in window configuration.
  :straight nil
  :bind (("C-c <right>" . winner-redo)
         ("C-c <left>". winner-undo))
  :hook (on-first-buffer . (lambda () (winner-mode 1))))

;; Scrolling

(use-package mwheel
  :straight nil
  :custom ((mouse-wheel-progressive-speed t)
           (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)
                                          ((meta))
                                          ((control meta) . global-text-scale)
                                          ((control) . text-scale)))))

(use-package pixel-scroll
  :straight nil
  ;; :hook (on-first-input . (lambda () (pixel-scroll-precision-mode +1)))
  :custom ((pixel-scroll-precision-interpolation-between-scroll 0.001)
           (pixel-scroll-precision-interpolation-factor 2.0)
           (pixel-scroll-precision-interpolation-total-time 0.1)
           (pixel-scroll-precision-large-scroll-height 15) ; 1.0
           (pixel-scroll-precision-momentum-min-velocity 10.0)
           (pixel-scroll-precision-momentum-seconds 1.75)
           (pixel-scroll-precision-momentum-tick 0.01)
           (pixel-scroll-precision-use-momentum t)))

(use-package ultra-scroll
  :straight (ultra-scroll :host github
                          :repo "jdtsmith/ultra-scroll")
  :custom ((scroll-conservatively 101)
           (scroll-margin 0))
  :hook (on-first-input . (lambda () (ultra-scroll-mode 1))))

(use-package yascroll
  :hook (on-first-buffer . (lambda () (global-yascroll-bar-mode 1))))

;; Configuration

(use-package activities
  :disabled
  :bind (("C-x C-a C-n" . activities-new)
         ("C-x C-a C-d" . activities-define)
         ("C-x C-a C-a" . activities-resume)
         ("C-x C-a C-s" . activities-suspend)
         ("C-x C-a C-k" . activities-kill)
         ("C-x C-a RET" . activities-switch)
         ("C-x C-a b" . activities-switch-buffer)
         ("C-x C-a g" . activities-revert)
         ("C-x C-a l" . activities-list))
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t))

;;; TABS

(use-package centaur-tabs
  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward))
  :custom ((centaur-tabs-style "chamfer")
           (centaur-tabs-set-icons t)
           (centaur-tabs-icon-type 'nerd-icons))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project))

;;; FILES

(use-package recentf
  :straight nil
  :bind ("C-x r F" . recentf)
  :hook (after-init . recentf-mode)
  :config
  (push "/\\.config/emacs/var/" recentf-exclude)
  (push "magit-diff.el" recentf-exclude)
  (push "magit-status.el" recentf-exclude))

(use-package save-place
  :straight nil
  :hook (after-init . save-place-mode))

;;; IMENU

(use-package imenu-list)

;;; BOOKMARK

(use-package bookmark+
  ;; Enhances vanilla Emacs bookmarks in many way.
  :defer 5
  :custom ((bookmark-save-flag 1))) ;; save more frequently, not just on Emacs quit

(provide 'subsys-navigation)
;;; subsys-navigation.el ends here
