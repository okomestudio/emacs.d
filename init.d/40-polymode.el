;;; 40-polymode.el --- Polymode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Framework for Multiple Major Modes in Emacs.
;;
;; Within an innermode region, M-n activates minor mode keymap.
;;
;;; Code:

(use-package polymode
  :init
  ;; See https://github.com/polymode/polymode/issues/316
  (setq-default polymode-lsp-integration nil))

;;; 40-polymode.el ends here
