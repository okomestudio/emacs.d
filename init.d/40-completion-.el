;;; 40-completion-.el --- completion  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Corfu and related utilities.
;;
;;; Code:

;; CAPE

(use-package cape
  :hook ((prog-mode
          text-mode
          conf-mode
          lsp-completion-mode) . ok-cape--set-super-capf)

  :custom
  (cape-dabbrev-check-other-buffers nil)

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
                          #'cape-file
                          #'tempel-complete
                          ;; #'tabnine-completion-at-point
                          #'cape-dabbrev)
                         :sort t
                         :exclusive 'no))))))

  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  ;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (add-to-list 'completion-at-point-functions #'cape-tex t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t))


(use-package tabnine
  ;; One-time `tabnine-install-binary' may be necessary.
  :disabled t                           ; Disabled till
                                        ; `tabnine-util--infer-indentation-offset'
                                        ; error gets resolved
  :commands (tabnine-start-process)
  :hook ((prog-mode . tabnine-mode)
         (kill-emacs . tabnine-kill-process))
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
  :straight (:host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :hook (((conf-mode
           prog-mode
           text-mode) . corfu-mode))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (corfu-scroll-margin 5)

  :init (global-corfu-mode)

  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (with-eval-after-load 'lsp-mode
    ;; Override the default, `:capf'
    (setq lsp-completion-provider :none))

  ;; For debugging; see the Corfu site README.
  (when debug-on-error
    (defun force-debug (func &rest args)
      (condition-case e
          (apply func args)
        ((debug error) (signal (car e) (cdr e)))))
    (advice-add #'corfu--post-command :around #'force-debug)))


(use-package corfu-info
  ;; `M-h' toggles the info on selected item.
  :disabled                             ; use popupinfo
  :straight nil)


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
  ;; (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))

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


(use-package prescient
  :autoload (prescient-persist-mode)
  :hook (corfu-mode . (lambda () (prescient-persist-mode 1)))
  :custom (prescient-aggressive-file-save t))


(use-package corfu-prescient
  :after corfu
  :hook (corfu-mode . (lambda () (corfu-prescient-mode 1)))
  :custom (corfu-prescient-enable-filtering nil))

;;; 40-completion-.el ends here
