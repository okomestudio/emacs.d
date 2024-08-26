;;; 02-minibuffer.el --- Minibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the minibuffer.
;;
;;; Code:

(use-package minibuffer
  :straight nil
  :custom ((completion-cycle-threshold 3)))

(use-package vertico
  ;; VERTical Interactive COmpletion.
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  :hook (after-init . vertico-mode))

(use-package embark
  ;; Emacs Mini-Buffer Actions Rooted in Key maps.
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  ;; Use if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer.
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  ;; Writable grep buffer and apply the changes to files.
  ;;
  ;; After embark export (`C-.' and `E' in consult minibuffer), `C-x
  ;; C-q' and `C-c C-p' to start editing. `C-c C-e' to apply changes.
  ;; `C-c C-k' to discard.
  )

(use-package marginalia
  ;; Marginalia in the minibuffer.
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

;;; 02-minibuffer.el ends here
