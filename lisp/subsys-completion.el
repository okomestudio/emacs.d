;;; subsys-completion-.el --- Completion Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the completion subsystem with Corfu.
;;
;;; Code:

(require 'ok)

;;; Cape

(use-package cape
  :hook ((prog-mode
          text-mode
          conf-mode
          lsp-completion-mode) . cape-ok--capf-set)
  :custom (cape-dabbrev-check-other-buffers nil)
  :config
  (require 'tempel)

  ;; `cape-capf-super' combines multiple capfs into one function to
  ;; speed up candidate lookup.
  (setq cape-ok--capf (cape-capf-noninterruptible
                       (cape-capf-buster
                        (cape-capf-properties
                         (cape-capf-super
                          ;; (car completion-at-point-functions)
                          #'cape-file
                          #'tempel-complete
                          ;; #'tabnine-completion-at-point
                          #'cape-dabbrev)
                         :sort t
                         :exclusive 'no))))
  (defun cape-ok--capf (&rest _) (apply cape-ok--capf _))
  (defun cape-ok--capf-set ()
    "Set CAPFs for the standard text/prog modes."
    (add-hook 'completion-at-point-functions #'cape-ok--capf -99 t))

  ;; Add the basic capfs with lower priorities here:
  (add-hook 'completion-at-point-functions #'cape-file 92)
  (add-hook 'completion-at-point-functions #'tempel-complete 93)
  ;; (add-hook 'completion-at-point-functions #'tabnine-completion-at-point)
  (add-hook 'completion-at-point-functions #'cape-tex 95)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 96)
  (add-hook 'completion-at-point-functions #'cape-keyword 97)
  (add-hook 'completion-at-point-functions #'cape-elisp-block 98))

;;; Templating

(use-package tempel
  :bind ( ("M-+" . tempel-complete)
          ("M-*" . tempel-insert)
          :map tempel-map
          ("M-}" . tempel-next)
          ("M-{" . tempel-previous) )
  :custom (tempel-path `(,(ok-file-expand-etc "tempel/templates.el")))
  :config
  (defun tempel-ok--include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  (add-to-list 'tempel-user-elements #'tempel-ok--include)

  (defun tempel-ok--include-file (elt fields)
    (when (eq (car-safe elt) 'include-file)
      (if-let ((filename (cadr elt))
               (content
                (with-current-buffer
                    (find-file-noselect
                     (expand-file-name
                      filename (ok-file-expand-etc "tempel")))
                  (save-restriction
                    (widen)
                    (buffer-substring-no-properties
                     (point-min)
                     (point-max))))))
          (let (pars k v)
            (dolist (item fields)
              (setq k (car item))
              (setq v (alist-get (cdr item) fields))
              (setq pars (append pars `((,k . ,(if v v (cdr item)))))))
            (ok-string-format content pars))
        (message "Template file %s not found" filename)
        nil)))
  (add-to-list 'tempel-user-elements #'tempel-ok--include-file)

  (defun tempel-ok--zenkaku (elt)
    "Handler for the `zenkaku' function.
This function detects the current input mode and renders the car of ELT
if hankaku is active and the cdr of ELT if zenkaku is active."
    (when (eq (car-safe elt) 'zenkaku)
      (if-let ((han (cadr elt))
               (zen (caddr elt)))
          (if (member current-input-method '("japanese-mozc")) zen han)
        (message "Bad arguments: %s" elt))))
  (add-to-list 'tempel-user-elements #'tempel-ok--zenkaku))

;;; COmpletion in Region FUnction (CORFU)

(use-package corfu
  :hook ((conf-mode prog-mode text-mode) . corfu-mode)
  :custom ((corfu-auto t)
           (corfu-auto-delay 0.5)
           (corfu-auto-prefix 1)
           (corfu-cycle t)
           (corfu-on-exact-match nil)
           (corfu-preselect 'prompt)
           (corfu-quit-no-match 'separator)
           (corfu-scroll-margin 5))
  :config
  (global-corfu-mode)
  (set-face-underline 'corfu-current t)

  (defun corfu-ok--enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil)  ; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ; disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (with-eval-after-load 'lsp-mode
    ;; Override the default, `:capf'
    (setq lsp-completion-provider :none))

  (defun corfu-ok--post-command (fun &rest args)
    "Post command hook for Corfu.
For tips on debugging, see the site readme."
    (if debug-on-error
        (condition-case e
            (apply fun args)
          ((debug error) (signal (car e) (cdr e))))
      (apply fun args)))
  (advice-add #'corfu--post-command :around #'corfu-ok--post-command))

(use-package corfu-history
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-history-mode))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay '(0.75 . 0.4)))

(use-package corfu-quick
  :straight nil
  :after corfu
  :bind ( :map corfu-map
          ("'" . corfu-quick-complete) ))

(use-package nerd-icons-corfu
  ;; An SVG alternative is `kind-icon'.
  :after corfu
  :hook (corfu-mode . (lambda () (require 'nerd-icons-corfu)))
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package org-block-capf
  ;; "<" trigger Org block completion at point.
  :straight (org-block-capf :host github :repo "xenodium/org-block-capf")
  :after corfu
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

;;; Orderless

(use-package orderless
  :custom ((completion-styles '(orderless basic))
           (completion-category-defaults nil) ; can comment this line out for defaults
           (completion-category-overrides '((file (styles partial-completion)))))
  :config
  ;; orderless-fast completion style:
  ;;
  ;; Uncomment the following block to use this completion style.
  ;;
  ;; See also https://tinyurl.com/mrybypkn for an advanced example.
  ;; ---
  ;; (defun ok-orderless-fast-dispatch (word index total)
  ;;   (and (= index 0) (= total 1) (length< word 4)
  ;;        `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
  ;; (orderless-define-completion-style orderless-fast
  ;;   (orderless-style-dispatchers '(ok-orderless-fast-dispatch))
  ;;   (orderless-matching-styles '(orderless-literal orderless-regexp)))
  ;; (setopt completion-styles '(orderless-fast basic))

  (with-eval-after-load 'corfu
    (add-hook 'corfu-mode-hook
              (lambda ()
                (setq-local orderless-matching-styles '(orderless-literal))))))

;;; Prescient

(use-package prescient
  :autoload (prescient-persist-mode)
  :hook (corfu-mode . prescient-persist-mode)
  :custom (prescient-aggressive-file-save t))

(use-package corfu-prescient
  :after corfu
  :hook (corfu-mode . corfu-prescient-mode)
  :custom (corfu-prescient-enable-filtering nil))

(provide 'subsys-completion)
;;; subsys-completion.el ends here
