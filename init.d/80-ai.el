;;; 80-ai.el --- AI  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize AI-related tools, including GPT clients.
;;
;;; Code:

(require 'okutil)


(use-package shell-maker
  :defer t

  :straight
  (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))


(use-package chatgpt-shell
  :defer t
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


(use-package gptel
  :defer t)


(use-package org-ai
  :defer t

  :custom
  (org-ai-default-chat-model "gpt-3.5-turbo")
  (org-ai-image-directory "~/Downloads/org-ai/")
  (org-ai-sd-directory "~/Downloads/org-ai/")

  :hook
  (org-mode . org-ai-mode)

  :commands
  (org-ai-mode
   org-ai-global-mode)

  :init
  (org-ai-global-mode)

  :config
  (org-ai-install-yasnippets))

;;; 80-ai.el ends here
