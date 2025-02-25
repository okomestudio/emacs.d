;;; subsys-company.el --- Company Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up Company subsystem.
;;
;;; Code:

(use-package company
  ;; Do not use `global-company-mode' when `lsp' completion is also in use.
  :bind (; See emacs.stackexchange.com/a/24800/599 for tips.
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
  :custom ((company-idle-delay 1.0)
           (company-minimum-prefix-length 2)
           (company-selection-wrap-around t)
           (company-show-numbers 'left)
           (company-tooltip-align-annotations t)
           (company-tooltip-limit 10)))

;; COMPANY FRONTENDS

(use-package company-quickhelp
  :disabled
  :bind (;
         :map company-active-map
         ("C-c h" . company-quickhelp-manual-begin))
  :custom (company-quickhelp-delay 2.0)
  :hook (company-mode . company-quickhelp-mode))

(use-package company-box
  :disabled
  :hook (company-mode . company-box-mode)
  :custom (company-box-doc-delay 1.0))

(use-package company-posframe
  :disabled
  :hook (company-mode . company-posframe-mode))

;; COMPANY BACKENDS

(use-package company-graphviz-dot
  :disabled
  :straight nil
  :after (graphviz-dot-mode)
  :init
  (ok-file-ensure-from-github
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
  :straight nil
  :after (company dash dash-functional tern)
  :init
  (ok-file-ensure-from-url
   (concat "https://gist.githubusercontent.com/"
           "okomestudio/de8c59960ce8f195ee0224de5db5a168/"
           "raw/1193992ffeeca8193ebf459b377c27f628ac3246/company-tern.el"))
  (add-to-list 'company-backends 'company-tern))

(use-package company-web
  :disabled
  :after company
  :init
  (add-to-list 'company-backends 'company-web-html))

(provide 'subsys-company)
;;; subsys-company.el ends here
