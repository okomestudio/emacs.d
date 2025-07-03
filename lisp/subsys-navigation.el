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

;;; Search & Movement

(use-package ace-isearch
  ;; A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
  :bind ( :map isearch-mode-map
          ("C-'" . ace-isearch-jump-during-isearch) )
  :custom ((ace-isearch-input-length 6)
           (ace-isearch-jump-delay 0.7)
           (ace-isearch-function #'avy-goto-char)
           (ace-isearch-function-from-isearch #'ace-isearch-consult-line-from-isearch))
  :hook (on-first-input . global-ace-isearch-mode))

(use-package avy)

;;; Windows & Frames

(use-package ace-window
  :bind ("M-O" . ace-window)
  :custom (aw-dispatch-always t))

(use-package frame
  :straight (:type built-in)
  :bind ( ("M-o" . next-window-any-frame)
          :repeat-map next-window-any-frame-repeat-map
          ("o" . next-window-any-frame)
          ("O" . previous-window-any-frame) )
  :config
  (put 'next-window-any-frame 'repeat-hint "o: next window")
  (put 'previous-window-any-frame 'repeat-hint "O: previous window"))

(use-package winner
  ;; Undo or redo a change in window configuration.
  :straight (:type built-in)
  :bind (("C-c <right>" . winner-redo)
         ("C-c <left>". winner-undo))
  :hook (on-first-buffer . winner-mode))

;;; Scrolling

(use-package emacs
  :custom ((scroll-conservatively 10)
           (scroll-margin 5)))

(use-package mwheel
  :straight (:type built-in)
  :custom ((mouse-wheel-progressive-speed t)
           (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)
                                          ((meta))
                                          ((control meta) . global-text-scale)
                                          ((control) . text-scale)))))

(use-package pixel-scroll
  :straight (:type built-in)
  ;; :hook (on-first-input . pixel-scroll-precision-mode)
  :custom ((pixel-scroll-precision-interpolation-between-scroll 0.001)
           (pixel-scroll-precision-interpolation-factor 2.0)
           (pixel-scroll-precision-interpolation-total-time 0.1)
           (pixel-scroll-precision-large-scroll-height 15) ; 1.0
           (pixel-scroll-precision-momentum-min-velocity 10.0)
           (pixel-scroll-precision-momentum-seconds 1.75)
           (pixel-scroll-precision-momentum-tick 0.01)
           (pixel-scroll-precision-use-momentum t)))

(use-package ultra-scroll
  :disabled
  :straight (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :custom ((scroll-conservatively 101)
           (scroll-margin 0))
  :hook (on-first-input . ultra-scroll-mode))

(use-package yascroll
  :hook (on-first-buffer . global-yascroll-bar-mode))

;;; Window Configuration

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

;;; Tabs

(use-package centaur-tabs
  :bind (("C-<prior>" . centaur-tabs-backward)
         ("C-<next>" . centaur-tabs-forward))
  :custom ((centaur-tabs-style "chamfer")
           (centaur-tabs-set-icons t)
           (centaur-tabs-icon-type 'nerd-icons))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project))

;;; Files

(use-package recentf
  :straight (:type built-in)
  :bind ("C-x r F" . recentf)
  :hook (after-init . recentf-mode)
  :config
  ;; Add regexp predicates for exclusion.
  (dolist (pred '("/\\.config/emacs/var/"
                  "magit-diff.el"
                  "magit-status.el"
                  ".*\\.elc$"))
    (push pred recentf-exclude)))

(use-package save-place
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

;;; Imenu

(use-package imenu-list)

;;; Bookmarks

(use-package bookmark+
  ;; Enhances vanilla Emacs bookmarks in many way.
  :defer 5
  :custom ((bookmark-save-flag 1))) ;; save more frequently, not just on Emacs quit

(provide 'subsys-navigation)
;;; subsys-navigation.el ends here
