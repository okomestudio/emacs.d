;;; lsp.el --- LSP  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; LSP configuration.
;;
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ensure-system-package (semgrep . "pip install semgrep")
  :custom ((lsp-completion-enable t)
           (lsp-diagnostics-provider :auto)
           (lsp-keymap-prefix "C-c l")
           (lsp-log-io ok-debug) ;; set to t for debugging
           (lsp-response-timeout 30)
           (lsp-enable-snippet nil)
           (lsp-use-plists t))
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :preface
  (put 'lsp-disabled-clients 'safe-local-variable #'listp)

  :config
  (setq lsp-headerline-arrow "➤")

  (defun lsp-workspace-shutdown-all ()
    "Shut down all running LSP servers.
The function returns LSP servers that have been shut down."
    (interactive)
    (let ((workspaces (lsp-workspaces)))
      (dolist (workspace workspaces)
        (lsp-workspace-shutdown workspace))
      workspaces)))

(use-package lsp-booster
  :straight (lsp-booster
             :host github
             :repo "okomestudio/lsp-booster.el"
             :post-build (("make")))
  :commands (lsp-booster-mode)
  :init (lsp-booster-mode 1))

(use-package lsp-ui
  ;; UI integration for lsp-mode.
  ;;
  ;; Note that unfocusing the doc frame leads to unresponsiveness.
  ;; Pressing an arrow key or tab goes out of that state. See
  ;; https://github.com/emacs-lsp/lsp-ui/issues/751.
  ;;
  :commands lsp-ui-mode
  :bind (;
         :map lsp-ui-mode-map
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
  :custom ((lsp-ui-doc-border "black")
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
           (lsp-ui-sideline-show-hover t))
  :config
  (set-face-background 'lsp-ui-doc-background "#eeeeee"))

(use-package lsp-treemacs
  :after (lsp-mode)
  :bind (([f6] . lsp-treemacs-symbols)
         ([f7] . lsp-treemacs-errors-list)))

(use-package consult-lsp
  :after (consult lsp-mode))

;;; lsp.el ends here
