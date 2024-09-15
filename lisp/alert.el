;;; alert.el --- Alert  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Alert configuration.
;;
;;; Code:

(use-package alert
  :demand t
  :custom ((alert-default-style 'notifications))
  :config
  (defvar notify-move-time nil
    "Timer object for alert. Use `cancel-timer' to suppress the alert.")

  (defun start-alert ()
    (run-with-timer 2400 2400
                    (lambda ()
                      (alert "Move away from computer!"
                             :title "MOVE!!"
                             :severity 'high))))

  (defun restart-alert ()
    (interactive)
    (cancel-timer notify-move-time)
    (setq notify-move-time (start-alert)))

  (setq notify-move-time (start-alert)))

;;; alert.el ends here
