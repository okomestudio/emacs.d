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

(use-package reformatter
  ;; Django template HTML formatter.
  ;;
  ;; To active per-project basis, have the following line in
  ;; .dir-locals.el:
  ;;
  ;;   (define-key web-mode-map (kbd "C-c b") #'djhtml-format-buffer)
  ;;
  :ensure-system-package (djhtml . "pip install djhtml")
  :config
  (reformatter-define djhtml-format
    ;; Provides `djhtml-format-buffer' and `djhtml-format-on-save-mode'.
    :program (locate-user-emacs-file "bin/djhtml")
    :args '("-")
    :stdin t
    :stdout t
    :lighter " DJHTML"
    :group 'djhtml-format))

(use-package lorem-ipsum)

(provide 'maj-web-mode)
;;; maj-web-mode.el ends here
