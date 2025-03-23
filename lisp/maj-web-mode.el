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
  ;; Django template HTML formatters.
  ;;
  ;; To active per-project basis, have the following line in
  ;; .dir-locals.el:
  ;;
  ;;   (define-key web-mode-map (kbd "C-c b") #'djhtml-format-buffer)
  ;;
  ;; (Or use `djlint-format-buffer'.)
  ;;
  :ensure-system-package
  (djhtml . "pip install djhtml")
  (djlint . "pip install djlint")
  :config
  (reformatter-define djhtml-format
    ;; Provides `djhtml-format-buffer' and `djhtml-format-on-save-mode'.
    :program (locate-user-emacs-file "bin/djhtml")
    :args '("-")
    :stdin t
    :stdout t
    :lighter " DJHTML"
    :group 'djhtml-format)

  (reformatter-define djlint-format
    ;; Provides `djlint-format-buffer' and `djlint-format-on-save-mode'.
    :program (locate-user-emacs-file "bin/djlint")
    :args '("--reformat"
            "--blank-line-after-tag" "load,extends"
            "--close-void-tags"
            "--format-css"
            "--format-js"
            "--ignore" "H006,H030,H031,T002"
            "--include" "H017,H035"
            "--indent" "2"
            "--max-line-length" "119"
            "--profile" "django"
            "-")
    :stdin t
    :stdout t
    :lighter " DJLINT"
    :group 'djlint-format))

(use-package lorem-ipsum)

(provide 'maj-web-mode)
;;; maj-web-mode.el ends here
