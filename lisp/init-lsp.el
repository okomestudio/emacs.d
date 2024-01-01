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
  ;; Note that unfocusing the doc frame leads to unresponsiveness.
  ;; Pressing an arrow key or tab goes out of that state. See
  ;; https://github.com/emacs-lsp/lsp-ui/issues/751.
  ;;
  :commands lsp-ui-mode

  :bind
  (:map lsp-ui-mode-map
        ("C-h ." . (lambda (arg)
                     (interactive "P")
                     (pcase arg
                       ('(4) (progn
                               (lsp-ui-doc-show)
                               ))
                       (_ (progn
                            (if (lsp-ui-doc--visible-p)
                                (lsp-ui-doc-focus-frame)
                              (lsp-ui-doc-glance)))))))
        :map lsp-ui-doc-frame-mode-map
        ("q" . (lambda ()
                 (interactive)
                 (lsp-ui-doc-unfocus-frame)
                 (lsp-ui-doc-hide))))

  :custom
  (lsp-ui-doc-border "black")
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-text-scale-level -1.0)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-sideline-delay 1.0)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)

  :config
  (set-face-background 'lsp-ui-doc-background "#eeeeee"))


(use-package lsp-treemacs
  :bind ([f7] . lsp-treemacs-symbols)
  :commands lsp-treemacs-errors-list)


(provide 'init-lsp)
;;; init-lsp.el ends here
