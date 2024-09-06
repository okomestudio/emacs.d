;;; openwith.el --- openwith  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure openwith and the related utilities.
;;
;;; Code:

(use-package openwith
  ;; Associate external applications with files.
  :custom (openwith-associations '(("\\.pdf\\'" "okular" (file))
                                   ("\\.svg\\'" "gwenview" (file))))
  :init
  (with-eval-after-load 'mm-util
    (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)))

;;; openwith.el ends here
