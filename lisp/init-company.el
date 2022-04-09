;;; init-company.el --- Company  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :custom
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers 'left)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 20)

  :init
  (global-company-mode)
  (add-to-list 'company-backends '(company-files))

  ;; See https://emacs.stackexchange.com/a/24800/599 for tweaking the enter key
  ;; behavior.
  (dolist (key '("<return>" "RET"))
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil)

  ;; Company appears to override the above keymap based on company-auto-complete-chars.
  ;; Turning it off ensures we have full control.
  (setq company-auto-complete-chars nil))

(use-package company-box
  :disabled
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors
   '((company-capf . (:candidate
                      (:foreground "black")

                      :selected
                      (:background "yellow" :foreground "black"))))))

(use-package company-graphviz-dot
  :after (graphviz-dot-mode)
  :ensure nil

  :init
  (ensure-file-from-github
   "ppareit/graphviz-dot-mode/master/company-graphviz-dot.el"))

(use-package company-jedi
  :disabled
  :after company
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package company-posframe
  :custom (company-posframe-font (font-spec :size (/ (* (face-attribute 'default :height) 1.5) 10)))
  :hook (company-mode . (lambda () (company-posframe-mode 1))))

(use-package company-restclient
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

(use-package company-tern
  :after (company dash dash-functional tern)
  :ensure nil
  :init
  (ensure-file-from-url
   (concat "https://gist.githubusercontent.com/"
           "okomestudio/de8c59960ce8f195ee0224de5db5a168/"
           "raw/1193992ffeeca8193ebf459b377c27f628ac3246/company-tern.el"))
  (add-to-list 'company-backends 'company-tern))

(use-package company-web
  :after company
  :init
  (add-to-list 'company-backends 'company-web-html))

(provide 'init-company)
;;; init-company.el ends here
