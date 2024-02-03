;;; 40-company.el --- Company  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  ;; See https://emacs.stackexchange.com/a/24800/599 for key binding tips.
  :defer t

  :bind
  (;
   :map company-mode-map
   ("<C-tab>" . company-indent-or-complete-common)

   :map company-active-map
   ("<tab>" . company-complete-selection)
   ("SPC" . nil)
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
  (company-tooltip-limit 20)

  :hook
  (after-init . global-company-mode))


;; Frontends

(use-package company-box
  :defer t

  :custom
  (company-box-backends-colors
   '((company-capf . (:all
                      (:foreground "black" :background "#eeeeee")
                      :selected
                      (:foreground "black" :background "#ffffff")))))

  :hook
  (company-mode . company-box-mode)

  :config
  (set-face-background 'company-box-background "#eeeeee"))


(use-package company-posframe
  :disabled

  :custom
  (company-posframe-font
   (font-spec
    :size (max (ts/default-font-size)
               (/ (* (face-attribute 'default :height)
                     1.5)
                  10))))

  :hook
  (company-mode . (lambda () (company-posframe-mode 1))))


;; Backends

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

;;; 40-company.el ends here
