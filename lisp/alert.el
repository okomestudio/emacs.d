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

  (setq notify-move-time
        (run-with-timer
         2400 2400
         (lambda ()
           (alert "Move away from computer!"
                  :title "MOVE!!"
                  :severity 'high)))))

;;; alert.el ends here
