;;; init-lsp-bridge.el --- lsp-bridge  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up lsp-bridge.
;;
;;; Code:

(use-package lsp-bridge
  :straight (lsp-bridge :type git
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
  :init (global-lsp-bridge-mode))

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
