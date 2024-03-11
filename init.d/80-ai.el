;;; 80-ai.el --- ai  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure AI and related tools, including GPT clients.
;;
;;; Code:

(use-package chatgpt-shell
  :straight (:host github :repo "xenodium/chatgpt-shell")
  :autoload (chatgpt-shell-send-to-buffer)
  :commands (ask-chatgpt)
  :custom
  (chatgpt-shell-model-version "gpt-3.5-turbo")
  (chatgpt-shell-openai-key (lambda ()
                              (auth-source-pick-first-password
                               :host "api.openai.com")))
  :config
  (defun ask-chatgpt (str)
    (interactive (list (ok-prompt-or-string-from-region "Ask ChatGPT: ")))
    (chatgpt-shell-send-to-buffer str)))


(use-package gptel
  :config
  (gptel-make-anthropic
      "Claude"
    :stream t
    :key (lambda ()
           (auth-source-pick-first-password
            :host "console.anthropic.com"))))


(use-package org-ai
  :disabled
  :custom
  (org-ai-default-chat-model "gpt-3.5-turbo")
  (org-ai-image-directory "~/Downloads/org-ai/")
  (org-ai-sd-directory "~/Downloads/org-ai/")

  :hook (org-mode . org-ai-mode)

  :commands
  (org-ai-mode
   org-ai-global-mode))

;;; 80-ai.el ends here
