;;; 40-polymode.el --- Polymode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Framework for Multiple Major Modes in Emacs.
;;
;; Within an innermode region, M-n activates minor mode keymap.
;;
;;; Code:

(use-package polymode
  :defer t

  :custom
  (polymode-lsp-integration nil) ;; see https://github.com/polymode/polymode/issues/316
  )

;;; 40-polymode.el ends here
