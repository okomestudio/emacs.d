;;; subsys-llm.el --- LLM Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up LLM subsystem.
;;
;; Related Packages:
;;
;;   - `chatgpt-shell'
;;
;;; Code:

(use-package gptel
  :config
  ;; The :key property does not have to be set explicitly here, when a 'machine'
  ;; entry for the API endpoint DNS name exists in authinfo file.
  (gptel-make-anthropic "Claude" :stream t)
  (gptel-make-deepseek "DeepSeek" :stream t)
  (gptel-make-xai "xAI" :stream t)
  (setopt gptel-model 'gemini-flash-latest
          gptel-backend (gptel-make-gemini "Gemini"
                          :key (lambda ()
                                 (auth-source-pick-first-password
                                  :host "generativelanguage.googleapis.com"))
                          :stream t)))

(use-package org-ai
  :bind ( :map org-ai-mode-map
          ("C-c r" . nil) )   ; prevent hijacking the key used for `org-ref'
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
