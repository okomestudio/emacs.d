;;; minibuffer.el --- Minibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The Minibuffer Configuration.
;;
;;; Code:

(use-package minibuffer
  :straight nil
  :custom ((completion-cycle-threshold 3)))

(use-package vertico
  ;; VERTical Interactive COmpletion.
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help))
  :custom ((vertico-count 20)
           (vertico-cycle t))
  :hook (on-first-input . vertico-mode))

(use-package embark-consult
  ;; Use if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer.
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  ;; Marginalia in the minibuffer.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :hook (vertico-mode . marginalia-mode))

(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode))

(use-package savehist
  ;; Save minibuffer history.
  :straight nil

  ;; NOTE: See which is more reliable
  ;; :hook (minibuffer-setup . savehist-mode)
  :init (savehist-mode 1)

  :config
  (with-eval-after-load 'org-roam
    (setopt savehist-additional-variables
            (append savehist-additional-variables
                    '(org-roam-ref-history)))))

;;; minibuffer.el ends here
