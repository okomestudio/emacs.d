;;; init-ace.el --- Ace  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ace-isearch - A seamless bridge between isearch, ace-jump-mode, avy, and swoop.
;; https://github.com/tam17aki/ace-isearch
(use-package ace-isearch
  :after (ace-jump-mode)
  :config (global-ace-isearch-mode 1)

  :custom
  (ace-isearch-function-from-isearch 'consult-line)
  (ace-isearch-input-length 6)
  (ace-isearch-jump-delay 0.75))

;; ace-jump-mode - A quick cursor jump mode
;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode)

(provide 'init-ace)
;;; init-ace.el ends here
