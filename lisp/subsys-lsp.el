;;; subsys-lsp.el --- LSP Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up LSP subsystem.
;;
;;; Code:

(use-package lsp-mode
  :custom ((lsp-completion-enable t)
           (lsp-diagnostics-provider :auto)
           (lsp-enable-snippet nil)
           (lsp-keymap-prefix "C-c l")
           (lsp-lens-place-position 'above-line)
           (lsp-log-io ok-debug)  ; set to t for debugging
           (lsp-response-timeout 30)
           (lsp-use-plists t))
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
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

(use-package lsp-mode
  ;; semgrep
  :custom ((lsp-semgrep-languages nil))
  :ensure-system-package (semgrep . "pip install semgrep"))

(use-package lsp-booster
  ;; NOTE: The `emacs-lsp-booster' binary is installed to the Emacs
  ;; binary directory (i.e., "which emacs"). The `make' may need to be
  ;; re-run on a new Emacs installation.
  ;;
  ;; To see if the mode is working, look at the error log of the
  ;; language server. When properly activated, the messages will
  ;; include entries tagged as "emacs_lsp_booster::app".
  :straight (lsp-booster :host github
                         :repo "okomestudio/lsp-booster.el"
                         :post-build (("make")))
  :commands (lsp-booster-mode)
  :hook (after-init . lsp-booster-mode))

(use-package lsp-ui
  ;; UI integration for lsp-mode.
  ;;
  ;; Note that unfocusing the doc frame leads to unresponsiveness.
  ;; Pressing an arrow key or tab goes out of that state. See
  ;; https://github.com/emacs-lsp/lsp-ui/issues/751.
  ;;
  :bind ( :map lsp-ui-mode-map
          ("C-h ." . lsp-ui-ok--toggle-doc)
          :map lsp-ui-doc-frame-mode-map
          ("q" . lsp-ui-ok--doc-quit) )
  :custom ((lsp-ui-doc-delay 0.2)
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
  :hook ((enable-theme-functions . lsp-ui-ok--theme))
  :commands lsp-ui-mode
  :config
  (defun lsp-ui-ok--toggle-doc (arg)
    "Toggle LSP UI help."
    (interactive "P")
    (pcase arg
      ('(4) (progn
              (lsp-ui-doc-show)))
      (_ (progn
           (if (lsp-ui-doc--visible-p)
               (lsp-ui-doc-focus-frame)
             (lsp-ui-doc-glance))))))

  (defun lsp-ui-ok--doc-quit ()
    "Quick LSP UI help."
    (interactive)
    (lsp-ui-doc-unfocus-frame)
    (lsp-ui-doc-hide))

  (defun lsp-ui-ok--theme (theme)
    (setopt lsp-ui-doc-border (face-attribute 'border :foreground nil 'default))
    (set-face-background 'lsp-ui-doc-background
                         (face-attribute 'default :background))))

(use-package lsp-treemacs
  :after (lsp-mode)
  :bind (([f6] . lsp-treemacs-symbols)
         ([f7] . lsp-treemacs-errors-list)))

(use-package consult-lsp
  :after (consult lsp-mode))

(load (ok-file-expand-lisp "pkg-lsp-grammarly.el"))
(load (ok-file-expand-lisp "pkg-lsp-ltex.el"))

(provide 'subsys-lsp)
;;; subsys-lsp.el ends here
