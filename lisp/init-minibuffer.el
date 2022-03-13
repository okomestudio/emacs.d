;;; init-minibuffer.el --- Minibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; vertico/consult
;;

;; vertico.el - VERTical Interactive COmpletion
;; https://github.com/minad/vertico
(use-package vertico
  :custom (vertico-count 20)
  :init (vertico-mode))

;; consult.el - Consulting completing-read
;; https://github.com/minad/consult
(use-package consult
  :config
  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any))

  :init
  (global-set-key (kbd "M-g b") 'consult-bookmark)
  (global-set-key (kbd "M-g f") 'consult-find)
  (global-set-key (kbd "M-g F") 'consult-locate)
  (global-set-key (kbd "M-g g") 'consult-git-grep)
  (global-set-key (kbd "M-g G") 'consult-grep)
  (global-set-key (kbd "M-g i") 'consult-imenu)
  (global-set-key (kbd "M-g I") 'consult-imenu-multi)
  (global-set-key (kbd "M-g l") 'consult-line)
  (global-set-key (kbd "M-g L") 'consult-line-multi)
  (global-set-key (kbd "M-g m") 'consult-mark)
  (global-set-key (kbd "M-g M") 'consult-gloabl-mark)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)
  (global-set-key [remap apropos] #'consult-apropos)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer] #'consult-buffer))

;; https://github.com/mohkale/consult-company
(use-package consult-company
  :disabled
  :after (consult company)
  :init (define-key company-mode-map [remap completion-at-point] #'consult-company))

;; https://github.com/minad/consult-flycheck
(use-package consult-flycheck
  :after (consult flycheck)
  :bind ("M-g c" . consult-flycheck))

;; https://gitlab.com/OlMon/consult-flyspell
(use-package consult-flyspell
  :after (consult flyspell)
  :bind ("M-g s" . consult-flyspell))

;; https://github.com/gagbo/consult-lsp
(use-package consult-lsp
  :after (consult lsp))

;; https://gitlab.com/OlMon/consult-projectile/
(use-package consult-projectile
  :after (consult projectile)
  :bind (([remap projectile-switch-project] . consult-projectile)
         ([remap projectile-find-file] . consult-projectile-find-file)
         ([remap projectile-find-dir] . consult-projectile-find-dir)))


;; embark.el - Emacs Mini-Buffer Actions Rooted in Key maps
;; https://github.com/oantolin/embark/
;;
;; Offers a hook to add relevant actions on a target determined by context.
;;
(use-package embark)

(use-package embark-consult
  :after (embark consult)
  ;:demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  ;:hook
  ;(embark-collect-mode . consult-preview-at-point-mode)
  )

;; marginalia.el - Marginalia in the minibuffer
;; https://github.com/minad/marginalia
(use-package marginalia
  :init (marginalia-mode))

;;
;; Misc.
;;

;; https://github.com/iyefrat/all-the-icons-completion
(use-package all-the-icons-completion
  :after (all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-mode))

;;Incremental minibuffer completion
(use-package icomplete
  :ensure nil)

;; orderless.el - Emacs completion style that matches multiple regexps in any order
;; https://github.com/oantolin/orderless
(use-package orderless
  :custom (completion-styles '(orderless)))

;; Save minibuffer history
(use-package savehist
  :init (savehist-mode))

;; Make unique buffer names more readable
(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
