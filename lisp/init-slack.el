;;; init-slack.el --- Slack  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package slack
  :commands (slack-start)

  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)

  :config
  (when (file-exists-p (concat user-emacs-directory "conf.d/slack.el"))
    (load (concat user-emacs-directory "conf.d/slack.el")))
  )

(provide 'init-slack)
