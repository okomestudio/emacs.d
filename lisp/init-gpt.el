;;; init-gpt.el --- GPT clients  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'okutil)


(use-package shell-maker
  :straight
  (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))


(use-package chatgpt-shell
  :requires shell-maker

  :straight
  (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))

  :custom
  (chatgpt-shell-openai-key (lambda ()
                              (auth-source-pick-first-password
                               :host "api.openai.com")))

  :init
  (defun ask-chatgpt (str)
    (interactive (list (okutil-string-from-region-or-prompt "Ask ChatGPT: ")))
    (chatgpt-shell-send-to-buffer str)))


(use-package gptel)


(provide 'init-gpt)
;;; init-gpt.el ends here
