;;; init-bookmark-plus.el --- Bookmark+  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package bookmark+
  ;; Enhances vanilla Emacs bookmarks in many way.
  :defer 2

  :custom
  (bmkp-bmenu-state-file (concat user-emacs-directory
                                 ".cache/bmk-bmenu-state.el"))
  (bmkp-bmenu-commands-file (concat user-emacs-directory
                                    ".cache/bmk-bmenu-commands.el"))
  (bmkp-last-as-first-bookmark-file nil)

  :init
  (require 'bookmark+))


(provide 'init-bookmark-plus)
;;; init-bookmark-plus.el ends here
