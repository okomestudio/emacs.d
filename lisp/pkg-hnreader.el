;;; pkg-hnreader.el --- hnreader  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure hnreader, Hacker News reader.
;;
;;; Code:

(use-package hnreader
  :bind ( :map reader-app-prefix-map
          ("h" . hnreader-news) )
  :config
  (defun hnreader-ok--apply-face-remappings ()
    "Apply buffer-local face remappings for `hnreader'."
    (when (member (buffer-name) '("*HN*" "*HNComments*"))
      (let ((height (face-attribute 'default :height)))
        (face-remap-add-relative 'org-level-1 :height height)
        (face-remap-add-relative 'org-level-2 :height height)
        (face-remap-add-relative 'org-level-3 :height height)
        (face-remap-add-relative 'org-level-4 :height height))))

  (add-hook 'org-mode-hook #'hnreader-ok--apply-face-remappings 98))

;; NOTE(2025-06-19): https://github.com/agzam/consult-hn might be of interest.

(provide 'pkg-hnreader)
;;; pkg-hnreader.el ends here
