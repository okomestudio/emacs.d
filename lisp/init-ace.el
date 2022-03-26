;;; init-ace.el --- Ace  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ace-isearch - A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
;; https://github.com/tam17aki/ace-isearch
(use-package ace-isearch
  :after (ace-jump-mode)

  :config
  (global-ace-isearch-mode 1)

  :init
  (defun ts/ace-isearch-function-from-isearch ()
    (consult-line isearch-string))

  :custom
  (ace-isearch-function-from-isearch 'ts/ace-isearch-function-from-isearch)
  (ace-isearch-input-length 6)
  (ace-isearch-jump-delay 0.75))

;; ace-jump-mode - A quick cursor jump mode
;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :after (help-swoop))

;; Install this just as ace-jump-mode dependency
(use-package helm-swoop)


(provide 'init-ace)
;;; init-ace.el ends here
