;;; init-ace.el --- Ace  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Based on the number of characters used for search, ace-isearch uses
;; different mode.
(use-package ace-isearch
  :after (ace-jump-mode)

  :config
  (global-ace-isearch-mode 1)

  :custom
  ((ace-isearch-input-length 20)
   (ace-isearch-jump-delay 0.75)))

(use-package ace-jump-mode
  :after (helm-swoop))

(provide 'init-ace)
;;; init-ace.el ends here
