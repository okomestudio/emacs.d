;;; subsys-minibuffer.el --- Minibuffer Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Minibuffer subsystem.
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

(use-package vertico-posframe
  :custom ((vertico-posframe-border-width 5)
           (vertico-posframe-min-width 75))
  :hook (on-first-input . (lambda () (vertico-posframe-mode 1))))

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
  :hook (after-init . savehist-mode)
  :config
  (with-eval-after-load 'org-roam
    (setopt savehist-additional-variables
            (append savehist-additional-variables
                    '(org-roam-ref-history)))))

(provide 'subsys-minibuffer)
;;; subsys-minibuffer.el ends here
