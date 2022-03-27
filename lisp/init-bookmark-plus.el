;;; init-bookmark-plus.el --- Bookmark+  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; bookmark+ - Enhances vanilla Emacs bookmarks in many way
;; https://www.emacswiki.org/emacs/BookmarkPlus#h5o-3
(use-package bookmark+
  :defer 2
  :ensure nil
  :init (require 'bookmark+)
  :quelpa
  (bookmark+ :fetcher wiki
             :files
             ("bookmark+.el"
              "bookmark+-mac.el"
              "bookmark+-bmu.el"
              "bookmark+-1.el"
              "bookmark+-key.el"
              "bookmark+-lit.el"
              "bookmark+-doc.el"
              "bookmark+-chg.el")) )

(provide 'init-bookmark-plus)
;;; init-bookmark-plus.el ends here
