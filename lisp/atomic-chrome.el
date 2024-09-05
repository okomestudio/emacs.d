;;; atomic-chrome.el --- Atomic Chrome  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Atomic Chrome is a Chrome extension that allows using Emacs for
;; bi-directional online editing.
;;
;; `M-x atomic-chrome-start-server' to start the server.
;;
;; Chrome extension:
;;
;;   https://chromewebstore.google.com/detail/dabdpcafiblbndpoadckibiaojbdnpjg
;;
;;; Code:

(use-package atomic-chrome
  :straight (atomic-chrome
             :repo "KarimAziev/atomic-chrome"
             :type git
             :flavor nil
             :host github)
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
               '("~/github.com/tmp/leetcode"
                 :url ("leetcode.com" "repl.it")))
  (add-to-list 'atomic-chrome-create-file-strategy
               '("~/github.com/tmp/medium"
                 :url ("medium.com")))
  (add-to-list 'atomic-chrome-create-file-strategy
               '("~/github.com/tmp/ts-scratch/src/"
                 :extension ("js" "ts" "tsx" "jsx" "cjs" "mjs"))))

;;; atomic-chrome.el ends here
