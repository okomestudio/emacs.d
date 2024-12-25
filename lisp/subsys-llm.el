;;; subsys-llm.el --- LLM Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up LLM subsystem.
;;
;;; Code:

(require 'ok)

(use-package chatgpt-shell
  :custom ((chatgpt-shell-openai-key (lambda ()
                                       (auth-source-pick-first-password
                                        :host "api.openai.com"))))
  :autoload (chatgpt-shell-send-to-buffer)
  :commands (ask-chatgpt)
  :config
  (defun ask-chatgpt (str)
    (interactive (list (ok-prompt-or-string-from-region "Ask ChatGPT: ")))
    (chatgpt-shell-send-to-buffer str))

  (setopt chatgpt-shell-root-path
          (no-littering-expand-var-file-name "chatgpt-shell")))

(use-package gptel
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda ()
           (auth-source-pick-first-password
            :host "console.anthropic.com"))))

(use-package org-ai
  :custom ((org-ai-default-chat-model "gpt-3.5-turbo")
           (org-ai-image-directory "~/tmp/org-ai/")
           (org-ai-sd-directory "~/tmp/org-ai/"))
  :hook ((org-mode . org-ai-mode)
         (org-mode . org-ai-ok--update-image-directory)
         (org-mode . org-ai-ok--update-sd-directory))
  :commands (org-ai-mode org-ai-global-mode)
  :config
  (defun org-ai-ok--set-output-directory (sym default &rest r)
    (set sym (or (and buffer-file-name
                      (file-name-directory buffer-file-name))
                 default)))

  (defalias 'org-ai-ok--update-image-directory
    (apply-partially #'org-ai-ok--set-output-directory
                     'org-ai-image-directory "~/tmp/org-ai"))

  (defalias 'org-ai-ok--update-sd-directory
    (apply-partially #'org-ai-ok--set-output-directory
                     'org-ai-sd-directory "~/tmp/org-ai")))

(provide 'subsys-llm)
;;; subsys-llm.el ends here
