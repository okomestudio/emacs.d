;;; 02-bookmark-plus.el --- Bookmark+  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package bookmark+
  ;; Enhances vanilla Emacs bookmarks in many way.
  :defer 7

  :custom
  (bmkp-bmenu-state-file (expand-file-name ".cache/bmk-bmenu-state.el"
                                           user-emacs-directory))
  (bmkp-bmenu-commands-file (expand-file-name ".cache/bmk-bmenu-commands.el"
                                              user-emacs-directory))
  (bmkp-last-as-first-bookmark-file nil))

;;; 02-bookmark-plus.el ends here
