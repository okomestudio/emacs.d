;;; 65-web-mode.el --- web-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)

  :bind
  (:map web-mode-map
        ("C-c b" . (lambda () (interactive) (prettier-js))))

  :hook
  (web-mode . init-webmode--set-up-flycheck)

  :mode
  ("\\.css\\'"
   "\\.html?\\'"
   "\\.html?\\.j2\\'" ;; Jinja2 HTML template
   "\\.jsx?\\'")

  :ensure-system-package
  (csslint . "npm install -g --save-dev csslint")
  (eslint . "npm install -g --save-dev eslint babel-eslint eslint-plugin-react")
  (tidy . "sudo apt install -y tidy")

  :config
  (defun init-webmode--set-up-flycheck ()
    "Customized web-mode-hoook."
    (add-node-modules-path)
    (require 'flycheck)

    ;; Disable checkers not in use
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist javascript-jshint javascript-jscs)))

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
             ;; (prettier-js-mode)
             ;; (lsp)
             ;; (tern-mode)
             ))

      (if (not checker)
          (message "Checker for flycheck does not exist for buffer")
        (message "Using %s for flycheck" checker)
        (flycheck-add-mode checker 'web-mode)
        (flycheck-select-checker checker))))

  (defun init-webmode--flyspell-predicate ()
    "Predicate for flyspell.

See http://blog.binchen.org/posts/effective-spell-check-in-emacs.html"
    (cond
     ;; for HTML buffer:
     ((string= web-mode-content-type "html")
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
                         web-mode-comment-face ;; focus on get html label right
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

         ;; Finalize the blacklist:
         (t (setq rlt nil)))

        ;; Return bool:
        rlt))

     ;; for JavaScript buffer:
     ((or (string= web-mode-content-type "javascript")
          (string= web-mode-content-type "jsx"))
      (let* ((f (get-text-property (- (point) 1) 'face)))
        ;; Only words with following font face will be checked (*whitelist*):
        (memq f '(js2-function-call
                  js2-function-param
                  js2-object-property
                  font-lock-variable-name-face
                  font-lock-string-face
                  font-lock-function-name-face))))))

  ;; (put 'web-mode 'flyspell-mode-predicate 'init-webmode--flyspell-predicate)
  )


(use-package typescript-mode)


(use-package prettier-js ;; or use web-beautify (?)
  :ensure-system-package
  (prettier . "npm install -g prettier")

  :config
  (setq prettier-js-args '("--arrow-parens" "always"
                           "--print-width" "88"
                           "--single-quote"
                           "--trailing-comma" "all")))


(use-package devdocs
  :hook
  (web-mode . (lambda () (setq-local devdocs-current-docs '("axios"
                                                            "css"
                                                            "dom"
                                                            "html"
                                                            "http"
                                                            "javascript"
                                                            "jinja~3.1"
                                                            "jquery"
                                                            "prettier"
                                                            "react"
                                                            "typescript"
                                                            "vite")))))


(use-package lsp-mode
  ;; Use ts-ls
  :hook
  (web-mode . (lambda ()
                (setq-local lsp-disabled-clients '(jsts-ls))
                (lsp-deferred)))

  :ensure-system-package
  (typescript-language-server . "npm install -g typescript-language-server typescript")

  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.html?\\.j2" . "html")))

;;; 65-web-mode.el ends here
