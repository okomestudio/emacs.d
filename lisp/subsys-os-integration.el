;;; subsys-os-integration.el --- OS Integration Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the OS integration subsystem.
;;
;;; Code:

(use-package atomic-chrome
  ;; Atomic Chrome is a Chrome extension that allows using Emacs for
  ;; bi-directional online editing.
  ;;
  ;; `M-x atomic-chrome-start-server' to start the server.
  ;;
  ;; Chrome extension:
  ;;
  ;;   https://chromewebstore.google.com/detail/dabdpcafiblbndpoadckibiaojbdnpjg
  ;;
  :commands (atomic-chrome-start-server)
  :defines atomic-chrome-create-file-strategy
  :config
  (setq-default atomic-chrome-auto-remove-file t)
  (setq-default atomic-chrome-buffer-open-style 'frame)
  (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
  (setq-default atomic-chrome-url-major-mode-alist
                '(("codesandbox.io" . js-ts-mode)
                  ("github.com" . gfm-mode)
                  ("gitlab.com" . gfm-mode)
                  ("jsfiddle.net" . js-ts-mode)
                  ("leetcode.com" . python-ts-mode)
                  ("ramdajs.com" . js-ts-mode)
                  ("typescriptlang.org" . typescript-ts-mode)
                  ("w3schools.com" . js-ts-mode)
                  ("zettelkasten.de" . gfm-mode)))
  (setopt atomic-chrome-default-major-mode 'python-ts-mode)

  (add-to-list 'atomic-chrome-create-file-strategy
               '("~/tmp/atomic-chrome/"
                 :url ("github.com"
                       "leetcode.com"
                       "medium.com"
                       "repl.it")))
  (add-to-list 'atomic-chrome-create-file-strategy
               '("~/tmp/atomic-chrome/"
                 :extension ("cjs" "js" "jsx" "mjs" "ts" "tsx"))))

(use-package browse-url
  :custom ((browse-url-browser-function 'browse-url-ok-browser-function)
           (browse-url-generic-program "xdg-open")
           (browse-url-handlers '(("localhost" . browse-url-generic))))
  :config
  (defun browse-url-ok-browser-function (url &optional arg)
    (interactive "sURL: \nP")
    (pcase arg
      ('(4) (browse-url-default-browser url))
      (_ (eww-browse-url url)))))

(use-package openwith
  ;; Associate external applications with files.
  :custom (openwith-associations '(("\\.pdf\\'" "okular" (file))
                                   ("\\.svg\\'" "gwenview" (file))))
  :init
  (with-eval-after-load 'mm-util
    (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)))

(provide 'subsys-os-integration)
;;; subsys-os-integration.el ends here
