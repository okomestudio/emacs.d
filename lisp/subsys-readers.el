;;; subsys-readers.el --- Website Readers  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure website reader applications.
;;
;;; Code:

(require 'ok)

(defvar ok-reader-buffers nil)

(use-package hnreader
  ;; Browse Hacker News in `org-mode'.
  ;;
  ;; NOTE(2025-06-19): https://github.com/agzam/consult-hn might be of
  ;; interest.
  :bind ( :map reader-app-prefix-map
          :prefix-map hnreader
          :prefix "h"
          ("b" . hnreader-best)
          ("j" . hnreader-jobs)
          ("n" . hnreader-news) )
  :config
  (dolist (buffer '("*HN*" "*HNComments*"))
    (add-to-list 'ok-reader-buffers buffer)))

(use-package reddigg
  ;; Browse Reddit in `org-mode'.
  :bind ( :map reader-app-prefix-map
          ("r" . reddigg-view-sub) )
  :config
  (dolist (buffer '("*reddigg*" "reddigg-comments*"))
    (add-to-list 'ok-reader-buffers buffer))

  (load (ok-file-expand-etc "reddigg/init")))

(defun ok-reader-buffers--init ()
  "Apply buffer-local face remappings for `hnreader'."
  (when (member (buffer-name) ok-reader-buffers)
    (let ((faces '( org-level-1 org-level-2 org-level-3 org-level-4
                    org-level-5 org-level-6 org-level-7 org-level-8 )))
      (dolist (face faces)
        (face-remap-add-relative face :weight 'normal)))))

(add-hook 'org-mode-hook #'ok-reader-buffers--init 98)

(provide 'subsys-readers)
;;; subsys-readers.el ends here
