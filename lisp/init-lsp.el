;;; init-lsp.el --- LSP  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package lsp-mode
  :commands lsp

  :custom
  (lsp-diagnostics-provider :auto)
  (lsp-log-io nil) ;; set to t for debugging
  (lsp-response-timeout 30)
  (lsp-use-plists t)

  :hook
  ((dockerfile-mode . (lambda () (init-lsp-lsp-mode-hook 'dockerfile-ls)))
   (json-mode . (lambda () (init-lsp-lsp-mode-hook 'json-ls)))
   (markdown-mode . lsp)
   (yaml-mode . (lambda () (init-lsp-lsp-mode-hook 'yamlls))))

  :preface
  (defun init-lsp-lsp-mode-hook (server)
    (lsp-ensure-server server)
    (lsp))

  (put 'lsp-disabled-clients 'safe-local-variable #'listp)

  :init
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  :config
  (setq lsp-headerline-arrow "➤"))


(use-package lsp-ui
  ;; UI integration for lsp-mode.
  ;;
  ;; Note that unfocusing the doc frame often leads to unresponsiveness.
  ;; Pressing an arrow key goes out of the state. See
  ;; https://github.com/emacs-lsp/lsp-ui/issues/715.
  ;;
  :commands lsp-ui-mode

  :bind
  (:map lsp-ui-mode-map
   ("C-F" . lsp-ui-doc-focus-frame)

   :map lsp-ui-doc-frame-mode-map
   ("q" . lsp-ui-doc-unfocus-frame))

  :custom
  (lsp-ui-doc-border "black")
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-max-height 10)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-text-scale-level -1.0)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-sideline-delay 1.0)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t))


(use-package lsp-treemacs
  :bind ([f7] . lsp-treemacs-symbols)
  :commands lsp-treemacs-errors-list)


(provide 'init-lsp)
;;; init-lsp.el ends here
