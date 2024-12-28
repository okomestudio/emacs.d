;;; maj-polymode.el --- Polymode Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the framework for handling multiple major modes.
;;
;; Within an innermode region, `M-n' activates minor mode keymap.
;;
;;; Code:

(use-package polymode
  :init
  ;; See https://github.com/polymode/polymode/issues/316
  (setq-default polymode-lsp-integration nil))

(provide 'maj-polymode)
;;; maj-polymode.el ends here
