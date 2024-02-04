;;; 02-scrolling.el --- Scrolling  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Window scrolling.
;;
;;; Code:

(use-package emacs ;; scroll configuration
  :defer t
  :straight nil

  :custom
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)
                                 ((meta))
                                 ((control meta) . global-text-scale)
                                 ((control) . text-scale)))
  (pixel-scroll-precision-interpolation-factor 1.2)
  (pixel-scroll-precision-large-scroll-height 1.0)
  (pixel-scroll-precision-momentum-min-velocity 0.5)
  (pixel-scroll-precision-momentum-seconds 0.5) ; 1.75
  (pixel-scroll-precision-momentum-tick 0.05)
  (pixel-scroll-precision-interpolation-total-time 0.1)
  (pixel-scroll-precision-interpolation-between-scroll 0.001)

  :hook
  (after-init . (lambda () (pixel-scroll-precision-mode +1))))


(use-package yascroll
  :defer t

  :hook
  (after-init . (lambda () (global-yascroll-bar-mode +1))))

;;; 02-scrolling.el ends here
