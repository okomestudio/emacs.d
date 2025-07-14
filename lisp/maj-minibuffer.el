;;; maj-minibuffer.el --- Minibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure minibuffer.
;;
;;; Code:

(use-package minibuffer
  :custom ((completion-cycle-threshold 3)
           (context-menu-mode t)
           (read-extended-command-predicate #'command-completion-default-include-p)
           (minibuffer-prompt-properties
            '(read-only t cursor-intangible t face minibuffer-prompt))
           (enable-recursive-minibuffers t)))

(use-package vertico
  ;; VERTical Interactive COmpletion.
  :bind ( :map vertico-map
          ("?" . minibuffer-completion-help) )
  :custom ((vertico-count 16)
           (vertico-cycle t))
  :hook ((after-init . vertico-mode)
         (vertico-mode . vertico-multiform-mode)))

(use-package vertico-posframe
  :if (display-graphic-p)
  :custom ((vertico-posframe-border-width 3)
           (vertico-posframe-min-width 75)
           (vertico-posframe-parameters '((left-fringe . 8)))
           (vertico-multiform-commands '((t (:not posframe)))))
  :hook (vertico-mode . vertico-posframe-mode))

(use-package marginalia
  ;; Marginalia in the minibuffer.
  :bind ( :map minibuffer-local-map
          ("M-A" . marginalia-cycle) )
  :hook (vertico-mode . marginalia-mode))

(use-package nerd-icons-completion
  :hook ((on-first-input . nerd-icons-completion-mode)
         (marginalia-mode . nerd-icons-completion-marginalia-setup)))

(use-package savehist
  ;; Save minibuffer history.
  :hook (after-init . savehist-mode))

(provide 'maj-minibuffer)
;;; maj-minibuffer.el ends here
