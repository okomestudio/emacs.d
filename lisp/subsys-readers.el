;;; subsys-readers.el --- Website Readers  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure website reader applications.
;;
;;; Code:

(defvar ok-reader-buffers nil)

(use-package hnreader
  ;; Browser Hacker News in `org-mode'.
  ;;
  ;; NOTE(2025-06-19): https://github.com/agzam/consult-hn might be of
  ;; interest.
  :bind ( :map reader-app-prefix-map
          ("h" . hnreader-news) )
  :config
  (dolist (buffer '("*HN*" "*HNComments*"))
    (add-to-list 'ok-reader-buffers buffer)))

(use-package reddigg
  ;; Browse Reddit in `org-mode'.
  :bind ( :map reader-app-prefix-map
          ("r" . reddigg-view-sub) )
  :config
  (dolist (buffer '("*reddigg*" "reddigg-comments*"))
    (add-to-list 'ok-reader-buffers buffer)))

(defun ok-reader-buffers--init ()
  "Apply buffer-local face remappings for `hnreader'."
  (when (member (buffer-name) ok-reader-buffers)
    (let ((height (face-attribute 'default :height)))
      (dolist (face '( org-level-1 org-level-2 org-level-3 org-level-4
                       org-level-5 org-level-6 org-level-7 org-level-8 ))
        (face-remap-add-relative face :height height :weight 'normal)))))

(add-hook 'org-mode-hook #'ok-reader-buffers--init 98)

(provide 'subsys-readers)
;;; subsys-readers.el ends here
