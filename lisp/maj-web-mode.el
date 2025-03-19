;;; maj-web-mode.el --- web-mode Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the web-mode major mode.
;;
;;; Code:

(use-package web-mode
  :custom ((web-mode-code-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-enable-auto-quoting nil)
           (web-mode-enable-current-column-highlight t)
           (web-mode-enable-current-element-highlight t)
           (web-mode-markup-indent-offset 2)
           (web-mode-script-padding 2)
           (web-mode-style-padding 2)))

(use-package lorem-ipsum)

(provide 'maj-web-mode)
;;; maj-web-mode.el ends here
