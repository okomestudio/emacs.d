;;; subsys-readers.el --- hnreader  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up website reader applications.
;;
;;; Code:

(defun ok--apply-face-remappings ()
  "Apply buffer-local face remappings for `hnreader'."
  (when (member (buffer-name) '("*HN*" "*HNComments*"))
    (let ((height (face-attribute 'default :height)))
      (face-remap-add-relative 'org-level-1 :height height)
      (face-remap-add-relative 'org-level-2 :height height)
      (face-remap-add-relative 'org-level-3 :height height)
      (face-remap-add-relative 'org-level-4 :height height)
      (face-remap-add-relative 'org-level-5 :height height)
      (face-remap-add-relative 'org-level-6 :height height)
      (face-remap-add-relative 'org-level-7 :height height)
      (face-remap-add-relative 'org-level-8 :height height))))

(add-hook 'org-mode-hook #'ok--apply-face-remappings 98)

(use-package hnreader
  ;; Browser Hacker News in `org-mode'.
  ;;
  ;; NOTE(2025-06-19): https://github.com/agzam/consult-hn might be of
  ;; interest.
  :bind ( :map reader-app-prefix-map
          ("h" . hnreader-news) ))

(use-package reddigg
  ;; Browse Reddit in `org-mode'.
  :bind ( :map reader-app-prefix-map
          ("r" . reddigg-view-sub) ))

(provide 'subsys-readers)
;;; subsys-readers.el ends here
