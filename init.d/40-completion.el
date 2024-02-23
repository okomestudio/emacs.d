;;; 40-completion.el --- completion  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure completion engines (Corf or Company) and related
;; utilities.
;;
;;; Code:

;; CAPE

(use-package cape
  :hook ((prog-mode
          conf-mode
          lsp-completion-mode) . ok-cape--set-super-capf)

  :custom (cape-dabbrev-check-other-buffers nil)

  :config
  (require 'tempel)

  (defun ok-cape--set-super-capf (&optional arg)
    (setq-local completion-at-point-functions
                (list (cape-capf-noninterruptible
                       (cape-capf-buster
                        (cape-capf-properties
                         (cape-capf-super
                          (if arg
                              arg
                            (car completion-at-point-functions))
                          #'tempel-complete
                          ;; #'tabnine-completion-at-point
                          #'cape-dabbrev
                          #'cape-file)
                         :sort t
                         :exclusive 'no))))))

  (add-to-list 'completion-at-point-functions #'tempel-complete)
  ;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-tex t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t))


(use-package tabnine
  ;; One-time `tabnine-install-binary' may be necessary.
  :disabled t                           ; Disabled till
                                        ; `tabnine-util--infer-indentation-offset'
                                        ; error gets resolved
  :commands (tabnine-start-process)
  :hook
  (prog-mode . tabnine-mode)
  (kill-emacs . tabnine-kill-process)

  :bind (nil
         :map tabnine-completion-map
	       ("TAB" . nil)
         ("<tab>" . nil))

  :config (tabnine-start-process))


;; TEMPLATING

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :custom
  (tempel-path `(,(no-littering-expand-etc-file-name "tempel/templates.el"))))


;; CORFU

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu"
                   :branch "async" :files (:defaults "extensions/*"))
  :bind
  (nil
   :map corfu-map
   ("TAB" . corfu-insert)
   ("<tab>" . corfu-insert)
   ("RET" . nil)
   ("<return>" . nil))

  :hook (prog-mode . corfu-mode)

  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  (tab-always-indent 'complete)

  :config
  (with-eval-after-load 'lsp-mode
    ;; Override the default, `:capf'
    (setq lsp-completion-provider :none)))


(use-package corfu-popupinfo
  :straight nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay '(0.75 . 0.4)))


(use-package nerd-icons-corfu
  ;; An SVG alternative is `kind-icon'.
  :after corfu
  :hook (corfu-mode . (lambda () (require 'nerd-icons-corfu)))
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides nil)
  (completion-category-defaults nil)

  :config
  ;; See https://tinyurl.com/mrybypkn for an advanced example.
  (with-eval-after-load 'corfu
    (add-hook 'corfu-mode-hook
              (lambda ()
                (setq-local orderless-matching-styles '(orderless-flex))))))


(use-package prescient
  :hook (corfu-mode . (lambda () (prescient-persist-mode 1)))
  :custom (prescient-aggressive-file-save t))


(use-package corfu-prescient
  :after corfu
  :hook (corfu-mode . (lambda () (corfu-prescient-mode 1)))
  :custom (corfu-prescient-enable-filtering nil))


;; COMPANY

(use-package company
  :disabled
  ;; Do not use `global-company-mode' when `lsp' completion is also in use.
  :bind
  (;; See https://emacs.stackexchange.com/a/24800/599 for key binding tips.
   :map company-mode-map
   ("C-<tab>" . company-indent-or-complete-common)

   :map company-active-map
   ("SPC" . nil)
   ;; See https://emacs.stackexchange.com/q/9631/599:
   ("<tab>" . company-complete-selection)
   ("TAB" . company-complete-selection)

   :map company-active-map
   :filter (company-explicit-action-p)
   ("<return>" . company-complete-selection)
   ("RET" . company-complete-selection))

  :custom
  (company-idle-delay 1.0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers 'left)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10))


;; COMPANY FRONTENDS

(use-package company-quickhelp
  :disabled
  :bind (:map company-active-map
              ("C-c h" . company-quickhelp-manual-begin))
  :custom (company-quickhelp-delay 2.0)
  :hook (company-mode . company-quickhelp-mode))


(use-package company-box
  :disabled
  :hook (company-mode . company-box-mode)
  :custom (company-box-doc-delay 1.0))


(use-package company-posframe
  :disabled
  :hook (company-mode . (lambda () (company-posframe-mode 1))))


;; COMPANY BACKENDS

(use-package company-graphviz-dot
  :disabled
  :after (graphviz-dot-mode)
  :straight nil

  :init
  (require 'okutil)
  (okutil-ensure-file-from-github
   "ppareit/graphviz-dot-mode/master/company-graphviz-dot.el"))


(use-package company-restclient
  :disabled
  :after (company restclient)

  :config
  (add-to-list 'company-backends 'company-restclient))


(use-package company-shell
  :disabled
  :after company

  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))


(use-package company-tern
  :disabled
  :after (company dash dash-functional tern)
  :straight nil

  :init
  (require 'okutil)
  (okutil-ensure-file-from-url
   (concat "https://gist.githubusercontent.com/"
           "okomestudio/de8c59960ce8f195ee0224de5db5a168/"
           "raw/1193992ffeeca8193ebf459b377c27f628ac3246/company-tern.el"))
  (add-to-list 'company-backends 'company-tern))


(use-package company-web
  :disabled
  :after company

  :init
  (add-to-list 'company-backends 'company-web-html))

;;; 40-completion.el ends here
