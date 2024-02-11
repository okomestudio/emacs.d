;;; 80-ai.el --- AI  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize AI-related tools, including GPT clients.
;;
;;; Code:

(use-package chatgpt-shell
  :straight (chatgpt-shell :host github :repo "xenodium/chatgpt-shell")

  :custom
  (chatgpt-shell-model-version "gpt-3.5-turbo")
  (chatgpt-shell-openai-key (lambda ()
                              (auth-source-pick-first-password
                               :host "api.openai.com"))))


(use-package gptel)


(use-package org-ai
  :custom
  (org-ai-default-chat-model "gpt-3.5-turbo")
  (org-ai-image-directory "~/Downloads/org-ai/")
  (org-ai-sd-directory "~/Downloads/org-ai/")

  :hook
  (org-mode . org-ai-mode)

  :commands
  (org-ai-mode
   org-ai-global-mode))

;;; 80-ai.el ends here
