;;; 02-minibuffer.el --- Minibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  ;; VERTical Interactive COmpletion.
  :custom
  (vertico-count 20)

  :init
  (vertico-mode))


(use-package embark
  ;; Emacs Mini-Buffer Actions Rooted in Key maps.
  ;;
  ;; Offers a hook to add relevant actions on a target determined by context.
  ;;
  :defer t
  :after embark-consult)


(use-package embark-consult
  ;:demand t ; only necessary if you have the hook below

  ;; Use if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer.
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package marginalia
  ;; Marginalia in the minibuffer.
  :init
  (marginalia-mode))


(use-package nerd-icons-completion
  :defer t

  :config
  (nerd-icons-completion-mode))


(use-package orderless
  ;; Emacs completion style that matches multiple regexps in any order.
  :defer t

  :custom
  (completion-styles '(orderless)))


(use-package savehist
  ;; Save minibuffer history.
  :defer t

  :init
  (savehist-mode))

;;; 02-minibuffer.el ends here