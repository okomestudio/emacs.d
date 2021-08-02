;;; init-webdev.el --- Webdev  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; JavaScript code formatter
(use-package prettier-js
  :ensure-system-package (prettier . "sudo npm install -g prettier")
  :config
  (setq prettier-js-args
        '(
          "--arrow-parens" "always"
          "--print-width" "88"
          "--single-quote"
          "--trailing-comma" "all"
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode for JavaScript, HTML, and CSS
;;
;; For linters, install the following:
;;
;;   $ sudo npm install -g eslint babel-eslint eslint-plugin-react
;;   $ sudo apt install tidy
;;   $ sudo npm install -g csslint
;;
(use-package web-mode
  :after (company-tern prettier-js add-node-modules-path)
  :mode ("\\.css\\'"
         "\\.html?\\'"
         "\\.j2\\'"
         "\\.jsx?\\'")

  :config
  (defun ts/web-mode-hook ()
    (add-node-modules-path)
    (require 'flycheck)
    ;; Disable checkers not in use
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist
                            javascript-jshint
                            javascript-jscs)))
    (let (checker)
      (cond ((string= web-mode-content-type "html")
             (when (executable-find "tidy")
               (setq checker 'html-tidy)))
            ((string= web-mode-content-type "css")
             (when (executable-find "csslint")
               (setq checker 'css-csslint)))
            ((or (string= web-mode-content-type "javascript")
                 (string= web-mode-content-type "jsx"))
             (when (executable-find "eslint")
               (setq checker 'javascript-eslint))
             (web-mode-set-content-type "jsx")
             (prettier-js-mode)
             (lsp)
             (tern-mode)))

      (flycheck-add-mode checker 'web-mode)
      (flycheck-select-checker checker)))

  (defun ts/web-mode-flyspell-verify ()
    ;; For detail, see:
    ;; http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
    (cond ((string= web-mode-content-type "html")
           (let* ((f (get-text-property (- (point) 1) 'face))
                  rlt)
             (cond
              ;; Check the words with these font faces, possibly.
              ;; This *blacklist* will be tweaked in next condition.
              ((not (memq f '(web-mode-html-attr-value-face
                              web-mode-html-tag-face
                              web-mode-html-attr-name-face
                              web-mode-constant-face
                              web-mode-doctype-face
                              web-mode-keyword-face
                              web-mode-comment-face  ;; focus on get html label right
                              web-mode-function-name-face
                              web-mode-variable-name-face
                              web-mode-css-property-name-face
                              web-mode-css-selector-face
                              web-mode-css-color-face
                              web-mode-type-face
                              web-mode-block-control-face)))
               (setq rlt t))
              ;; Check attribute value under certain conditions:
              ((memq f '(web-mode-html-attr-value-face))
               (save-excursion
                 (search-backward-regexp "=['\"]" (line-beginning-position) t)
                 (backward-char)
                 (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                         (thing-at-point 'symbol)))))
              ;; Finalize the blacklist.
              (t (setq rlt nil)))
             rlt))
          ((or (string= web-mode-content-type "javascript")
               (string= web-mode-content-type "jsx"))
           (let* ((f (get-text-property (- (point) 1) 'face)))
             ;; *whitelist*
             ;; only words with following font face will be checked
             (memq f '(js2-function-call
                       js2-function-param
                       js2-object-property
                       font-lock-variable-name-face
                       font-lock-string-face
                       font-lock-function-name-face))))))

  (put 'web-mode 'flyspell-mode-predicate 'ts/web-mode-flyspell-verify)

  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)

  :ensure-system-package
  (;(csslint . "npm install --save-dev csslint")
   ;(eslint . "npm install --save-dev eslint babel-eslint eslint-plugin-react")
   (tidy . "sudo apt install tidy"))

  :hook (web-mode . ts/web-mode-hook)

  :init
  (add-to-list 'company-backends '(company-css)))

(use-package typescript-mode)

(provide 'init-webdev)
;;; init-webdev.el ends here
