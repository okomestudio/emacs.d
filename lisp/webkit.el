;;; webkit.el --- webkit  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; WebKit, aka a fully fledged browser inside Emacs.
;;
;;; Code:

;; NOTE: xwidget-webkit doesn't work since webkit2gtk version 2.42.1.
;;
;; See mail.gnu.org/archive/html/bug-gnu-emacs/2023-09/msg01905.html

(use-package webkit
  :straight (webkit
             :type git
             :host github
             :repo "akirakyle/emacs-webkit"
             :branch "main"
             :files (:defaults "*.js" "*.css" "*.so")
             :pre-build ("make")))

;;; webkit.el ends here
