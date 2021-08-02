;;; init-minibuffer.el --- Minibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package consult
  :init
  (global-set-key [remap goto-line] 'consult-goto-line))

(use-package embark)

(use-package embark-consult
  :after (embark consult)
  ;:demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  ;:hook
  ;(embark-collect-mode . consult-preview-at-point-mode)
  )

;; minibuffer completion incremental feedback
(use-package icomplete
  :ensure nil)

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  ;; Completion style that matches multiple regexps in any order
  :init
  (setq completion-styles '(orderless)))

(use-package savehist
  :init
  (savehist-mode))

;; Make unique buffer names more readable
(use-package uniquify
  :ensure nil
  :custom
  ;; '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
  (uniquify-buffer-name-style 'forward))

(use-package vertico
  :custom
  (vertico-count 20)

  :init
  (vertico-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
