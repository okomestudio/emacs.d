;;; lsp-bridge.el --- lsp-bridge  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Fast, multi-threaded LSP client.
;;
;;; Code:

(use-package posframe
  :straight t
  :after dashboard)

(use-package markdown-mode
  :straight t
  :after dashboard)

(use-package lsp-bridge
  :straight (lsp-bridge
             :type git
             :host github
             :repo "manateelazycat/lsp-bridge"
             :files (:defaults "*.el"
                               "*.py"
                               "acm"
                               "core"
                               "langserver"
                               "multiserver"
                               "resources")
             :build (:not compile))
  :init
  (global-lsp-bridge-mode))

;;; lsp-bridge.el ends here
